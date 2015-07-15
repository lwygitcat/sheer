/** \file uncaught-except.c
 *
 * Report exception escaping "main" functions or functions that
 * exclude them from their throw specification.
 *
 * \author Zaur Kambarov
 * \date 01/03/06
 * (c) 2006-2015 Coverity, Inc. All rights reserved worldwide.
 **/

#include "text/string-list.hpp" // split

#include "sm/checker.hpp"
#include "sm/state/empty-state.hpp"
#include "uncaught-except.hpp"
#include "types/extend-types.hpp"
#include "types/types.hpp"
// getThrownExnEvents, add_throw_events
#include "analysis/checkers/generic/derivers/relax-uncaught-except/derive-relax-uncaught-except-impl.hpp"
#include "symbols/function.hpp"
#include "traversal/abstract-interp.hpp"
#include "analysis/sm/sm-option.hpp"             // sm_option_t
#include "analysis/work-unit/work-unit.hpp"
#include "analysis/work-unit/wu-analysis.hpp"
#include "analysis/work-unit/wu-global-info.hpp" // WUGlobalInfo::enabledSMs

#include "modelling/model-patterns.hpp" // relax_uncaught_except_sm_name

#include "libs/containers/output.hpp" // printable_container
#include "libs/exceptions/user-error.hpp"        // throw_XUserError
#include "libs/regex/regex.hpp" // printable_container
#include "libs/structured-text/st-elements.hpp"  // createCode
#include "libs/time/activity-path-timer.hpp" // START_STOP_ACTIVITY

#include "ast/ast/exception-handling.hpp"        // isIgnoredExceptionType

// Regular expression that indicates which functions are considered
// "main" functions, and should have exceptions reported by default it
// they're roots.
static Regex main_regex("((((^|_)m|M)ain)|(^MAIN))$");

// This is a rather strange animal that, on the one hand, is flow-insensitive,
// doesn't track values, and uses no FPP, but, on the other hand, uses models.
// It is not a normal tree-walking checker -- it does its own traversal.
// It would be nice if there were an appropriate base class to capture this.
class uncaught_except_t : public checker_t
{
public:
    // Constructor
    uncaught_except_t() :
        checker_t("UNCAUGHT_EXCEPT"),
        report_all_fun_opt(false),
        report_exn_spec(true),
        report_thrown_pointers(false),
        follow_indirect_calls(false)
    {
        if(g_aggressiveness_level >= AGGRESSIVE_HIGH) {
            report_all_fun_opt = true;
        }
    }

    virtual void init_once() {
        checker_t::init_once();
        cxx_checker_ran = true;
    }


    void init_fn(state_t* sm_st,
                 abstract_interp_t &cur_traversal);

    // Create an empty state.
    state_t* create_empty_state(mc_arena a);

    void handle_tree
        (state_t *state,
         const ASTNode *t,
         abstract_interp_t &cur_traversal);

    // Print out the error messages.
    void final_fn(abstract_interp_t &cur_traversal,
                  ostream &out,
                  error_set_t &errors);

    bool want_traversal() const {
        return true;
    }

    override TU_LANG_SET languages_wanted() const {
        return TU_LANG_SET::singleton(TU_LANG_CXX);
    }

    // We don't need to process the results here; we're not generating
    // any models

    void get_dependencies(vector<string> &dependencies);

    void checker_enablement_data(EnablementData &d, LanguageFamily lang)
    {
        d.disposition = ED_DEFAULT;
    }

    override LanguageFamilySet supportedLanguages() const
    {
        return LANG_FAMILY_SET_CLIKE;
    }

    void  output_uncaught_for_fun(const WUAnalysis &fn);
    void  create_uncaught_model(const WUAnalysis &fn);
    void get_options(vector<sm_option_t *> &options);

    //variables for handling options

    // If set, functions whose identifier is matched by the regex will
    // not have reports (except for exceptionreturn truei specification
    // violations) even if they're roots
    scoped_ptr<Regex> fun_ignore;
    // If set, functions whose identifier is matched by the regex will
    // have reports for all escaping exceptions, even if not roots
    scoped_ptr<Regex> fun_report;
    // If set, do not report exceptions whose identifier is matched by
    // the regex.
    scoped_ptr<Regex> except_ignore;
    // If set, only report exceptions exceptions whose identifier is
    // matched by the regex and which are not ignored
    scoped_ptr<Regex> except_report;
    // If true, report all root functions (except if ignored)
    bool  report_all_fun_opt;
    // If true, report exception specification violations (DR
    // 1685). Defaults to true.
    bool  report_exn_spec;
    // If true, traverse the AST tree and report thrown pointer-to-exception.
    bool report_thrown_pointers;
    // If true (default is false), allow reporting bugs that involve
    // indirect calls (virtual/function pointer resolution)
    bool follow_indirect_calls;

    static bool regex_type_match
    (const type_t *t,
     const Regex &regex) {
        const class_type_t *c = t->as_class_p();
        if(!c || !c->get_id()) {
            return false;
        }
        return regex.search(c->get_id());
    }

    static bool is_globally_ignored(const type_t *t) {
        return isIgnoredExceptionType(t);
    }

    bool report_exn_type
    (const type_t *t) {
        const class_type_t *c = t->as_class_p();
        if(!c) {
            // Report on all non-class exceptions
            return true;
        }
        const char *id = c->get_id();
        if(!id) {
            // Supposed to mean an anonymous class.
            return false;
        }
        // Special case for "bad_alloc"
        // It's not a normal exclude: when "except_report" is used, it
        // only applies to exception specification violations. When
        // except_ignore is set, it doesn't apply at all.
        return
            except_report ?
            except_report->search(id) :
            (except_ignore || !is_globally_ignored(t));
    }

};

OPEN_ANONYMOUS_NAMESPACE;

struct class_list_opt_t : public sm_option_t
{
    class_list_opt_t(const string &name,
                     scoped_ptr<Regex> &regex,
                     bool ids_partial):
        sm_option_t(name),
        regex(regex),
        ids_partial(ids_partial)
    {
    }

    void output_type_cur_xml() const {
        output_regex_type();
    }

    void handleMaster(
        AnalysisMasterData &masterData,
        bool firstTime,
        const string &checker,
        const string &masterValue,
        string &workerValue,
        bool &remove) {
        handleWorker(workerValue);
    }
    void handleWorker(const string &value) {
        // Backwards compatibility:
        // if a comma-separated list of identifiers, match the set of
        // identifiers
        bool is_comma_list = true;
        foreach(i, value) {
            if(!isalnum(*i) && *i != '_'
               && *i != ',') {
                is_comma_list = false;
                break;
            }
        }
        ostringstream ostr;
        if(regex) {
            ostr << regex->asString() << '|';
        }
        ostr << "(?:";
        if(is_comma_list) {
            if(!ids_partial)
                ostr << "^(?:";
            foreach(i, value) {
                char c = *i;
                if(c == ',')
                    c = '|';
                ostr << c;
            }
            if(!ids_partial)
                ostr << ")$";
        } else {
            ostr << value;
        }
        ostr << ')';
        string regex_value = ostr.str();
        if(!regex) {
            regex.reset(new Regex(regex_value));
        } else {
            *regex = ostr.str();
        }
        if(!regex->isValid()) {
            throw_XUserError(
                stringb(
                    "Invalid regular expression for UNCAUGHT_EXCEPT:"
                    << name
                    << ": "
                    << value)
            );
        }
    }
    scoped_ptr<Regex> &regex;
    bool const ids_partial;
};

CLOSE_ANONYMOUS_NAMESPACE;

void uncaught_except_t::get_options(vector<sm_option_t *> &options)
{
#define CLASS_LIST_OPT(name, ids_partial) \
    options.push_back(                          \
        new class_list_opt_t(#name, name, ids_partial))

    CLASS_LIST_OPT(fun_ignore, /*ids_partial*/true);
    CLASS_LIST_OPT(fun_report, /*ids_partial*/true);
    CLASS_LIST_OPT(except_ignore, /*ids_partial*/false);
    CLASS_LIST_OPT(except_report, /*ids_partial*/false);
    BOOL_OPT(report_exn_spec);
    BOOL_OPT(report_thrown_pointers);
    bool_opt(options, "report_all_fun", report_all_fun_opt);
    BOOL_OPT(follow_indirect_calls);
    // The deriver should inherit "follow_indirect_calls"
    copyTo(options.back(), relax_uncaught_except_sm_name);
}

state_t* uncaught_except_t::create_empty_state(mc_arena a) {
    return new (a) empty_state_t (a, this);
}

void uncaught_except_t::init_fn(state_t* state,
                                abstract_interp_t &cur_traversal) {
    const function_t *f = cur_traversal.current_fnsym();
    const char *fn_id = f->get_id();

    const function_type_t *ftype = f->get_ftype();

    bool has_exn_spec = report_exn_spec && ftype->has_exn_spec();

    // Indicates whether this is a "root equivalent" function, i.e. a
    // function in which we'll report all thrown exceptions
    bool is_root = false;

    // Is this a function we specifically want to report? (in which
    // case it doesn't matter if it's really a root)
    if(fun_report) {
        if(fun_report->search(fn_id)) {
            is_root = true;
        }
    }
    if(!cur_traversal.get_wu().hasCallers) {
        // Root function
        if(report_all_fun_opt) {
            // We want to report all roots
            is_root = true;
        } else if(!fun_report) {
            // We didn't specifiy which functions we wanted to report,
            // so report main-like root functions.
            if(main_regex.search(fn_id)) {
                is_root = true;
            }
        }
    }
    // Try ignore last: it overrides the rest
    if(is_root && fun_ignore) {
        is_root = !fun_ignore->search(fn_id);
    }

    if (!has_exn_spec && !is_root) {
        return;
    }

    class_loader_t &class_loader =
        cur_traversal.get_type_loader();

    multimap<const type_t *,
        vector<const Expression *>,
        type_t::ptr_lt_t> thrown_types;
    vector<const Expression *> rethrow_events;

    getThrownExnEvents(
        cur_traversal,
        thrown_types,
        rethrow_events,
        follow_indirect_calls);

    event_manager_t *event_manager =
        &cur_traversal.event_manager;

    foreach(i, thrown_types) {

        const type_t *t = i->first;

        CDOUT("Exception type escapes function: " << *t);

        bool exn_spec_violation =
            has_exn_spec
            &&
            // Ignore bad_alloc, unless except_ignore is set
            (except_ignore || !is_globally_ignored(t))
            &&
            !ftype->exception_may_be_thrown(t, class_loader);

        if(!
           // Report if exn not in spec
           (exn_spec_violation
            ||
            // Report if it's a function on which we want to report
            // *and* it's an exception type we want to report.
            (is_root && report_exn_type(t))
            )
           ) {
            CDOUT("Type is allowed, skipping");
            continue;
        }
        // Try ignore last, it overrides the rest, and applies to spec
        // violation (unlike "except_report")
        if(except_ignore && regex_type_match(t, *except_ignore)) {
            continue;
        }

        err_t *error = create_nopath_error(cur_traversal);
        if(exn_spec_violation) {
            STStream str;
            STPrintEnv env(str, PrintOptions::defaults(cur_traversal.getLang()));
            f->output_exn_spec(env);

            error->add_event
                ("exn_spec_violation",
                 desc() << "An exception of type " << *i->first
                 << " is thrown but the throw list "
                 << createCode(str.finish_as_fragment())
                 << " doesn't allow it to be thrown. "
                 "This will cause a call to unexpected() "
                 "which usually calls terminate().",
                 cur_traversal.current_fn_decl_loc());
        } else {
            error->add_event
                ("root_function",
                 desc() << "In function " << f
                 << " an exception of type "
                 << *i->first
                 << " is thrown and never caught.",
                 cur_traversal.current_fn_decl_loc());
        }
        error->set_main_event();
        error->set_extra_extract_class(i->first);

        add_throw_events(event_manager,
                         i->first,
                         i->second,
                         error);
        commit_error(error, cur_traversal);
    }
}

void uncaught_except_t::handle_tree(state_t* state,
                                 const ASTNode *t,
                                 abstract_interp_t &cur_traversal)
{
    BEGIN_PATTERNS;
    static AnyType type;
    static Pointer p(type);
    static Throw throw_ptr(p);
    static ModelPattern uncaught(relax_uncaught_except_property);
    END_PATTERNS;

    if (throw_ptr.match(t)) {
        CDOUT ("matched throw ptr: " << p);
        err_t *error = create_nopath_error(cur_traversal);
        event_stream &es = desc();
        es << "A pointer of " << type << " is thrown, which may be unintentional. "
            "It is recommended to 'Throw by Value, Catch by Reference' in C++.";
        error->add_event("throw_pointer", es, t);
        error->set_extra_extract_class(type.last_type());
        error->set_main_event();
        commit_error(error, cur_traversal);
    }
    else if (uncaught.match(t)) {
    #if 0
        if (cur_traversal.current_block()->incoming_exn_context.is_present()) {
            // some callee threw an exception while one is already pending
//            if (const char *typeStr = uncaught.get_option_with_kind("type_as_string", MOK_TYPE)) {
            
            const  types::type_t* extype = NULL;
            if (const char *typeStr = uncaught.find_type_option("type_as_string")) {
                extype = cur_traversal.fn_trav().string_to_type(typeStr);
            }
            if (!extype) {
                return;
            }
            
      #endif     
           CDOUT ("inside the supposed area!");
            err_t *err = create_nostate_error(cur_traversal);
            event_stream &es = desc();
            err->add_event("pending", 
                           es << "An exception is thrown while another is already pending.", 
                           t,
                           uncaught.get_model_tree());
         //   err->set_extra_extract_class(type); temp comment out
            err->set_main_event();
            commit_error(err, cur_traversal);
//        }
   }
}

// uncaught_except_t::final_fn
//
// Uniqify the error messages and print them out.
//
void uncaught_except_t::final_fn(abstract_interp_t &cur_traversal,
                                 ostream &out,
                                 error_set_t &errors)
{
    group_errors(errors, "uncaught_except");
    checker_t::final_fn(cur_traversal, out, errors);
}

void uncaught_except_t::get_dependencies(vector<string> &dependencies) {
    dependencies.push_back(relax_uncaught_except_sm_name);
}

NamableSM *create_uncaught_except() {
    return create_checker_nofpp(new uncaught_except_t);
}
