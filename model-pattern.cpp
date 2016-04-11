// Copyright (c) 2006-2010,2012-2015 Synopsys, Inc. All rights reserved worldwide.
#include "model-pattern.hpp"

#include "exceptions/assert.hpp"

#include "ast/cc.ast.hpp"
#include "ast/symbols/function.hpp"  // function_t

#include "model-manager.hpp"
#include "work-unit/wu-analysis.hpp" // g_current_function_trav

namespace AST_patterns {
//
// class ModelPattern
//

ModelPattern::ModelPattern(const char *property)
    : property(get_global_string_manager()->get_cstr_with_null(property)),
      last_model_tree(0),
      last_call(0)
{
}

void ModelPattern::print(ostream& out) const{
    out << "ModelPattern(" << (property ? property : "") << ")";
}

const function_t *ModelPattern::get_called_fn() const {
    string_assert(last_model_tree, "called get_called_fn with no model tree match");
    return last_model_tree->called_fn;
}

void ModelPattern::set_matched(const E_model *model_tree,
                               const E_funCall *call_tree) {
    last_model_tree = model_tree;
    last_call = call_tree;
}

bool ModelPattern::matches(const Expression *t)
{
    const E_model *m = t->ifE_modelC();
    if(!m) {
        return false;
    }

    last_model_tree = m;

    if(property && last_model_tree->model->property != property)
        return false;
    
    last = last_model_tree->iface;
    last_call = last_model_tree->call;
    return true;
}

const Expression *ModelPattern::get_interface() const{
    return last_expr();
}
const action_interface_t *ModelPattern::get_action_iface() const{
    return model().iface;
}

const E_model *ModelPattern::get_model_tree() const {
    return last_model_tree;
}

bool ModelPattern::has_model() const {
    return last_model_tree != 0;
}

const model_action_t &ModelPattern::model() const {
    return *last_model_tree->model;
}

unsigned ModelPattern::num_options() const {
    return model().getAllOptions().size();
}

const E_funCall *ModelPattern::get_call() const{
    return last_call;
}

const Expression *ModelPattern::get_func() const{
    return last_call->func;
}

const char *ModelPattern::get_property() const {
    return last_model_tree->model->property;
}

const char *ModelPattern::find_string_option(const char *name) const
{
    return model().find_string_option(name);
}

const char *ModelPattern::find_type_option(const char *name) const
{
    return model().find_type_option(name);
}

bool ModelPattern::has_flag_option(const char *name) const
{
    return model().has_flag_option(name);
}

bool ModelPattern::find_int_option(const char *name, sint64 &val) const
{
    return model().find_int_option(name, val);
}

const parse_errors::ModelOptionValue *ModelPattern::find_any_option(
    const char *name) const
{
    return model().find_any_option(name);
}

const action_interface_t *ModelPattern::find_iface_option(
    const char *name) const
{
    return model().find_iface_option(name);
}

//
// class ModelPatternArg
//

ModelPatternArg::ModelPatternArg(const char *property) :
    ModelPattern(property) {
}

void ModelPatternArg::print(ostream& out) const {
    out << "ModelPatternArg(" << (property ? property : "") << ")";
}

static int findIfacePos(const action_interface_t *iface)
{
    ASTSWITCH(iface) {
        ASTCASE1(function_interface_t) {
            return -1;
        }
        ASTCASE1(return_interface_t) {
            return -1;
        }
        ASTCASE(param_interface_t, p) {
            return p->n;
        }
        ASTCASE(field_interface_t, f) {
            return findIfacePos(f->obj);
        }
        ASTCASE(deref_interface_t, d) {
            return findIfacePos(d->ptr);
        }
        ASTCASE1(global_interface_t) {
            return -1;
        }
        ASTCASE1(alloc_interface_t) {
            return -1;
        }
        ASTCASE1(sink_interface_t) {
            return -1;
        }
        ASTCASE1(source_interface_t) {
            return -1;
        }
        ASTCASE1(concat_interface_t) {
            return -1;
        }
        ASTCASE1(conststr_interface_t) {
            return -1;
        }
        ASTCASE1(attribute_interface_t) {
            return -1;
        }
        ASTENDCASE;
    }
    xfailure("Invalid interface");
}

int ModelPatternArg::pos() const
{
    const model_action_t &m = model();
    return findIfacePos(m.iface);
}

bool ModelPatternArg::is_receiver_object() const
{
    if (const function_t *f = get_called_fn()) {
        return f->is_nonstatic_method() && pos()==0;
    }
    return false;
}

Expression const *ModelPatternArg::get_argument() const
{
    E_funCall const *call = get_call();
    Expression const *ret = call->getArgOrNull(pos());
    if (!ret) {
        // As the question on the implementation of 'pos' indicates,
        // I'm unsure of whether this can happen.  For now, I'll be
        // defensive and return the interface expression, which is
        // usually similar to the argument expression.
        ret = get_interface();
    }
    return ret;
}


class ModelPatternWithArg: public ExpressionPattern {
public:
    ModelPatternWithArg(ModelPatternArg &m, ExpressionPattern &arg):m(m),arg(arg){}
    void print(ostream &out) const {
        out << printable(m) << '(' << printable(arg) << ')';
    }
protected:
    bool matches(const Expression *e) {
        if(m.match(e)) {
            return arg.match(m.get_interface());
        } else {
            return false;
        }
    }
private:
    ModelPatternArg &m;
    ExpressionPattern &arg;
};

ExpressionPattern &ModelPatternArg::operator()(ExpressionPattern &p) {
    return *new(current_pattern_arena) ModelPatternWithArg(*this, p);
}

//
// class NoFun
//

void NoFun::print(ostream& out) const {
    out << "NoFun";
}

bool NoFun::matches(const Expression *e)
{
    CallSite call;
    if(!call.match(e))
        return false;
    emit_model_type_t model_type;
    return !g_current_function_trav->hasModel
        (call.last_expr(),
         get_module_name(module),
         model_type);
}


}
