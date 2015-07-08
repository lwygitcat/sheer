public:      // checker_t funcs
    override state_t* create_empty_state(mc_arena a);
    override bool want_casts() const;
    override void get_options(vector<sm_option_t *> &options);
    override void get_dependencies(vector<string> &dependencies);
    override void init_once();
    override void init_fn(state_t           *state,
                          abstract_interp_t &cur_traversal);
    override void final_fn(abstract_interp_t &cur_traversal);
    override void handle_tree(state_t *state, const ASTNode *t, abstract_interp_t &cur_traversal);
    void my_handle_tree(ASTNode const *t);

    bool want_traversal() const {
        return false;
    }

    void checker_enablement_data(EnablementData &d, LanguageFamily lang)
    {
        d.disposition = ED_DEFAULT;
    }

    override LanguageFamilySet supportedLanguages() const
    {
        return
            LANG_FAMILY_SET_CLIKE | LanguageFamilySet::fromValues(LANG_FAMILY_JAVA, LANG_FAMILY_CSHARP, LANG_FAMILY_JAVASCRIPT);
    }

private:
    bitset_t eval(Expression const *);

    string exprContext(Expression const *);

    err_t *createErrorWithEvent(const char *tag,
                                const char *subcategory,
                                string const &msg);
    err_t *createErrorWithEventAndContext(const char *tag,
                                          const char *subcategory,
                                          string const &msg,
                                          Expression const *expr_for_context);
    err_t *createErrorWithEventAndElaboration(const char *tag,
                                              const char *subcategory,
                                              string const &msg,
                                              string const &elaboration);
    err_t *createErrorWithEventAndContextAndElaboration(
        const char       *tag,
        const char       *subcategory,
        string const     &msg,
        Expression const *expr_for_context,
        string const     &elaboration);
    err_t *createErrorWithEventAndContextAndElaborations(
        const char       *tag,
        const char       *subcategory,
        string const     &msg,
        Expression const *expr_for_context,
        string const     &elaboration,
        string const     &elaboration2);

    bool reportBitAndWithZero() const;
    bool reportConstantLogicalOperands() const;

    void rememberReportingError();

    err_t *checkBitwise(Expression const *,
                        Expression const *,
                        Expression const *,
                        BinaryOp);

    // &&/|| used where &/| intended?
    err_t *checkBitwiseVsLogicalAndOr(Expression const *expr,
                                      Expression const *e1,
                                      Expression const *e2,
                                      bool is_andand);

    err_t *checkJavaScriptTypeofComparison(Expression const *expr,
                                           E_javascriptTypeof const *typof,
                                           Expression const *otherSide,
                                           BinaryOp op,
                                           abstract_interp_t *cur_traversal);

    bitset_t redo(
        BinaryOp op,
        bitset_t const &E1,
        bitset_t const &E2
    );

    // If "biny" is a comparison of an unsigned variable vs
    // 0, causing the condition to be constant, then return true.
    // These should be reported by NO_EFFECT instead.
    // See bug 41166.
    bool isUnsignedCompareZero(const E_binary *biny);

    mc_arena arena; // meaningful only during my_handle_tree()
    set<const Expression *> *reported;

#if CER_USE_STORE
    // The union of the stores created during AI.  This is the store consulted
    // by the actual checking done in my_handle_tree, in order to achieve
    // path-insensitive results.
    std::map<Expression const *,
             bitset_t,
             astnode_lt_t,
             arena_obj_alloc_t<std::pair<Expression const *, bitset_t> > unionStore;
#endif

public: // for visitor
    Expression const *curTree; // meaningful only during my_handle_tree()
    abstract_interp_t *curTraversal; // meaningful only during my_handle_tree()
    TU_LANG curLang; // meaningful only during my_handle_tree()

private:
    bool recursiveIsConstOrLiteral(Expression const *orig_e);
    bool isConstOrLiteral(Expression const *e);
};

#define MESSAGE(m) consume_event_stream(desc() << m)

// "Elaboration" message, as second or later sentence.  This adds
// the space between sentences, which CovLStrv1 did implicitly.
#define EMESSAGE(m) MESSAGE(" " << m)

string toStr(BinaryOp x, TU_LANG l) {return string(toString(x, l));}

OPEN_ANONYMOUS_NAMESPACE;

// different than in bitfpp -- less conservative
struct cer_bitset_widen {
    void operator()(state_t       *,
                    ASTNode const *,
                    bitset_t       &storeVal,
                    bitset_t const &cacheVal) const
    {
        if (storeVal.compare_full(cacheVal) != 0 &&
            storeVal.typeParms == cacheVal.typeParms) {
            cout << "widening " << storeVal << " and " << cacheVal << " to ";
            if (storeVal.isAnyBoolean() && cacheVal.isAnyBoolean()) {
                // special case for different values of Booleans along different
                // paths -- don't lose the fact that they're Boolean
                storeVal = bitset_t::booleanBottom(storeVal.typeParms);
            } else {
                storeVal.bottomOut();
            }
            cout << storeVal << endl;
        } else {
            cout << storeVal << " and " << cacheVal << " widen to bottom" << endl;
            storeVal.bottomOut();
        }
    }
};

CLOSE_ANONYMOUS_NAMESPACE;


typedef store_t<
    bitset_t,
    hash_and_count_unique_t<
        bitset_t,
        ShallowCopy<bitset_t>,
        bitset_t::full_hash,
        bitset_t::full_eq>,
    cer_bitset_widen> bit_store_t;


ConstantExpressionResult::ConstantExpressionResult()
  : checker_t("CONSTANT_EXPRESSION_RESULT"),
    report_bit_and_with_zero_langs(TU_LANG_SET_ALL_VM),
    report_bit_and_with_zero_in_macros(false),
    report_constant_logical_operands_langs(TU_LANG_SET::none()),
    report_constant_logical_operands_in_macros(false),
    report_unnecessary_op_assign(false),

    // these are just to shut up UNINIT_CTOR
    arena(NULL),
    reported(NULL),
    curTree(NULL),
    curTraversal(NULL),
    curLang(TU_LANG_C) // This will get reset to the correct lang before my_handle_tree


#if CER_USE_STORE
    ,
    unionStore(new std::map<Expression const *, bitset_t, astnode_lt_t, arena_obj_alloc_t<std::pair<Expression const *, bitset_t> >(ar))
#endif
{
    VXCOV;
    if(g_aggressiveness_level >= AGGRESSIVE_MED) {
        VXCOV;
        report_constant_logical_operands_langs = TU_LANG_SET::all();
        report_bit_and_with_zero_langs = TU_LANG_SET::all();
    }
    if(g_aggressiveness_level >= AGGRESSIVE_HIGH) {
        VXCOV;
        report_constant_logical_operands_in_macros = true;
        report_bit_and_with_zero_in_macros = true;
        report_unnecessary_op_assign = true;
    }
}

state_t* ConstantExpressionResult::create_empty_state(mc_arena a)
{
    VXCOV;
#if CER_USE_STORE
    return new (a) bit_store_t (a, this);
#else
    return new (a) empty_state_t (a, this);
#endif
}


bool ConstantExpressionResult::want_casts() const
{
    VXCOV;
    return true;
}

void ConstantExpressionResult::get_options(vector<sm_option_t *> &options)
{
    bool_opt(options, "report_bit_and_with_zero", report_bit_and_with_zero_langs);
    BOOL_OPT(report_bit_and_with_zero_in_macros);
    onlyListFor(options.back(), SourceLanguageSet::fromValues(SL_C, SL_CXX));
    bool_opt(options, "report_constant_logical_operands",
             report_constant_logical_operands_langs);
    BOOL_OPT(report_constant_logical_operands_in_macros);
    onlyListFor(options.back(), SourceLanguageSet::fromValues(SL_C, SL_CXX));
    BOOL_OPT(report_unnecessary_op_assign);
}

bool ConstantExpressionResult::reportBitAndWithZero() const
{
    return report_bit_and_with_zero_langs.contains(curTraversal->getLang());
}

bool ConstantExpressionResult::reportConstantLogicalOperands() const
{
    return hasSymbolicConstants(curTraversal->getLang())
        && report_constant_logical_operands_langs.contains(curTraversal->getLang());
}

void ConstantExpressionResult::init_once() {
    checker_t::init_once();
    if (report_bit_and_with_zero_in_macros) {
        report_bit_and_with_zero_langs.add(TU_LANG_SET_ALL_CLIKE);
    }
    if (report_constant_logical_operands_in_macros) {
        report_constant_logical_operands_langs.add(TU_LANG_SET_ALL_CLIKE);
    }
}

static ASTNode *parentSkippingIntLit(ASTNode const *t)
{
    ASTNode *result = t->parent;
    if (result->isExpression()) {
        Expression *parentExpr = result->asExpression();
        if (parentExpr->isE_intLit()) {
            VXCOV;
            result = parentExpr->parent;
        } else {
            VXCOV;
        }
    } else {
        VXCOV;
    }

    return result;
}


inline Expression const *skipBoolification(Expression const *e,
                                           bool compiler_generated)
{
    // Match "_ == 0" or "_ != 0", where only the latter form occurs
    // implicitly.
    if (E_binary const *biny = e->ifE_binaryC()) {
        if (((!compiler_generated &&
              (biny->op == BIN_EQUAL || biny->op == BIN_DYNAMIC_EQUAL))/*1*/ ||
             biny->op == BIN_NOTEQUAL || biny->op == BIN_DYNAMIC_NOTEQUAL) /*2*/&&
             (!compiler_generated || biny->isImplicit == PI_IMPLICIT)) {
            E_intLit const *int_lit = biny->e2->ifE_intLitC();
            E_floatLit const *float_lit = biny->e2->ifE_floatLitC();
            if ((int_lit && int_lit->i == 0) ||
                (float_lit /* && assume it is 0.0 */)) {
                VXCOV;
                return skipBoolification(biny->e1, compiler_generated);
            }
        } else {
            VXCOV;
        }
    } else {
        // Match the same things pushed down under an E_intLit node.
        if (E_intLit const *intLit = e->ifE_intLitC()) {
            if (intLit->original_expr) {
                Expression const *possible =
                    skipBoolification(intLit->original_expr,
                                      compiler_generated);
                if (possible != intLit->original_expr) {
                    VXCOV;
                    return possible;
                } else {
                    VXCOV;
                }
            } else {
                VXCOV;
            }
        } else {
            VXCOV;
        }
    }

    return e;
}

inline Expression const *skipCompilerGeneratedBoolification(Expression const *e)
{
    return skipBoolification(e, /*compiler_generated*/ true);
}

Expression const *skipEnclosingCompGenBoolification(Expression const *e)
{
    Expression *parentExpr = parentSkippingIntLit(e)->ifExpression();
    if (parentExpr != NULL &&
        skipBoolification(parentExpr, /*compiler_generated*/ true) == e) {
        VXCOV;
        return parentExpr;
    }

    VXCOV;
    return e;
}


inline bool isCompilerGeneratedBoolification(Expression const *e)
{
    VXCOV;
    return skipBoolification(e, /*compiler_generated*/ true) != e;
}

inline bool isBoolification(Expression const *e)
{
    VXCOV;
    return skipBoolification(e, /*compiler_generated*/ false) != e;
}


// Is 'e' the '_' in a compiler-generated "_ != 0" construct?
static bool inCompilerGeneratedBoolificationContext(Expression const *e)
{
    if (Expression const *parentExpr =
        parentSkippingIntLit(e)->ifExpressionC()) {
        VXCOV;
        return isCompilerGeneratedBoolification(parentExpr);
    }

    VXCOV;
    return false;
}


inline bool isAnyLit(Expression const *e)
{
    VXCOV;
    // why are there casts that are not pushed down under literal nodes?
    e = strip_casts(e);
    bool result = e->isE_intLit() || e->isE_floatLit() || e->isE_stringLit() ||
                  (e->isE_addrOf() /* && is a fixed addrsss */) ||
                  e->isE_predefinedConstantLit();
    if (!result) {
        //cout << "non-literal " << e->kindName() << " " << *e << endl;
    }
    return result;
}

static const Expression *extractFoldedExpr (const Expression *oe)
{
     if(const E_intLit *ilit = oe->ifE_intLitC())
         if (ilit->original_expr)
             return ilit->original_expr;
     return oe;
}

// Assuming we already know that 'orig_e' is a constant expression evaluating
// to a particular desired value, is 'orig_e' composed only of things other
// than logical operations (&&, ||, !)?
static bool recursiveContainsNoLogical(Expression const *orig_e)
{
    Expression const *e = strip_casts(orig_e);
    ASTSWITCH(e) {
    ASTCASE(E_binary, bny) {
        switch (bny->op) {
        case BIN_AND:
            VXCOV; goto and_or_case;
        case BIN_OR:
            VXCOV;
and_or_case:
            return false;

        default:
            VXCOV;
            return true;
        }
    }
    ASTCASE(E_unary, uny) {
        if (uny->op == UNY_NOT) {
            // special case for !!, which is idiomatically used to turn
            // a multi-bit value into a single-bit with the same Boolean
            // sense; "!!<literal>" results in a tree of the form
            // (! (literal original_expr: (! (literal ...)) ...))

            // If a ! node is constant it always has an intLit child.
            //
            // This is not true. I was given an AST for !(i|1) without
            // constant-folding (the intLit child):
            // E_unary(!E_binary(E_binary(i|1)!=0))
            // Thus extra work is needed to extract folded expression.
            const Expression *child = extractFoldedExpr(uny->expr);
            if (E_unary const *sub_uny = child->ifE_unaryC()) {
                if (sub_uny->op == UNY_NOT) {
                    GDOUT (checkers, "Double NOT: " << *(sub_uny->expr));
                    return recursiveContainsNoLogical(sub_uny->expr);
                }
            }
            return false;
            /*
            cond_assert(uny->expr->isE_intLit());
            E_intLit const *sub_intLit = uny->expr->asE_intLitC();
            if (sub_intLit->original_expr) {
                if (E_unary const *sub_uny =
                    sub_intLit->original_expr->ifE_unaryC()) {
                    if (sub_uny->op == UNY_NOT) {
                        VXCOV;
                        return recursiveContainsNoLogical(sub_uny->expr);
                    } else {
                        VXCOV;
                    }
                } else {
                    VXCOV;
                }
            } else {
                VXCOV;
            }
            return false;*/
        }

        VXCOV;
        return true;
    }

    ASTCASE(E_cond, cond) {
        VXCOV;
        // (**) see above
        // Strictly speaking, we should only really care about the branch
        // selected by the constant cond node.
        return recursiveContainsNoLogical(cond->cond) &&
               (recursiveContainsNoLogical(cond->th) ||
                recursiveContainsNoLogical(cond->el));
    }

    ASTCASE(E_dup, dup) {
        return recursiveContainsNoLogical(dup->reused);
    }

    ASTCASE(E_intLit, int_lit) {
        if (int_lit->original_expr) {
            VXCOV;
            return recursiveContainsNoLogical(int_lit->original_expr);
        }
        VXCOV;
        return true;
    }
    ASTCASE(E_predefinedConstantLit, lit) {
        if (lit->original_expr) {
            VXCOV;
            return recursiveContainsNoLogical(lit->original_expr);
        }
        VXCOV;
        return lit->constant != CONSTANT_TRUE
            && lit->constant != CONSTANT_FALSE;
    }
    ASTCASE1(E_variable) {
        VXCOV;
        // Under a literal, a "variable" can only be a C++ constant.
        return true;
    }
    ASTENDCASED;
        // not sure what else might come up here...
    }

    VCOV;
    return true;
}


inline bool containsNoLogical(Expression const *e)
{
    VXCOV;
    return recursiveContainsNoLogical(e);
}


// Is 'orig_e' (a cast of) (a constant expression evaluating to) the same
// Boolean value (0, non-0) as 'x'?  If 'immediate' we require that there be
// a literal buried directly under this crud, otherwise we allow that it could
// be an expression limited by 'alowLogical' and 'allowBitwise' [as per
// containsNo()].
static bool recursiveHasLiteralX(Expression const *orig_e,
                                 int               x,
                                 bool              immediate,
                                 bool              allowLogical,
                                 bool              underLiteral = false)
{
    Expression const *e = skipCompilerGeneratedBoolification(orig_e);
    ASTSWITCH(e) {
    ASTCASE(E_cast, cast) {
        scalar_type_t const *scal_type;
        //cout << "cast: " << *cast << endl;
        if (cast->ckind == CK_IMPLICIT ||

            ((scal_type = cast->type->as_scalar_p()) &&
             scal_type->is_integral()) ||

            (cast->type->as_pointer_p() && x == 0)) {
            //cout << "recursing " << (immediate ? "shallowly" : "deeply") << " on cast operand " << *cast->expr << endl;
            VXCOV;
            return recursiveHasLiteralX(cast->expr, x, immediate, allowLogical,
                                        underLiteral);
        }

        //if (cast->type->as_scalar_p()) {cout << "to non-integral scalar " << endl;}
        //else {cout << "to non-scalar, non-pointer ";}
        //cout << " type " << *cast->type << endl;

        VCOV;
        return false;
    }

    ASTCASE(E_intLit, int_lit) {
        // We're not looking for strict numerical identity here -- we're looking
        // for equivalent Boolean values.
        if ((int_lit->i == 0) == (x == 0)) {
            if (int_lit->original_expr == NULL) {
                //cout << "no original_expr ==> true" << endl;
                VXCOV;
                return true;
            }
            //cout << "recursing " << (immediate ? "shallowly" : "deeply") << " on intLit's original_expr " << *int_lit->original_expr << endl;
            if (immediate ? recursiveHasLiteralX(int_lit->original_expr, x,
                                                 immediate, allowLogical,
                                                 /*underLiteral*/ true) :
                            allowLogical || containsNoLogical(int_lit->original_expr)) {
                //cout << "that worked" << endl;
                VXCOV;
                return true;
            } else {
                VCOV;
            }
        } else {
            VXCOV;
        }
        //if (int_lit->original_expr) {cout << "original_expr: " << *int_lit->original_expr << endl;}
        //else {cout << "value: " << int_lit->i << endl;}
        return false;
    }

    ASTCASE1(E_variable) {
        VXCOV;
        // the only "variables" under literals are C++ constants
        return underLiteral;
    }

    ASTENDCASED;
    }

    VXCOV;
    //cout << "not intLit " << e->kindName() << endl;
    return false;
}


inline bool hasLiteralX(Expression const *e,
                        int               x,
                        bool              immediate,
                        bool              allowLogical)
{
    VXCOV;
    return recursiveHasLiteralX(e, x, immediate, allowLogical);
}


// To test whether \p e is exactly literal x
inline bool isLiteralX(Expression const *e, int x)
{
    if (const E_intLit *ilit = e->ifE_intLitC()) {
        if (!ilit->original_expr && ilit->i == x) {
            return true;
        }
    }
    return false;

}
inline bool isLiteral0(Expression const *e)
{
    return isLiteralX(e, 0);
}
inline bool isLiteral1(Expression const *e)
{
    return isLiteralX(e, 1);
}


#if NOT_CURRENTLY_USED
inline bool isLiteralBoolean(Expression const *e)
{
    return isLiteral0(e) || isLiteral1(e);
}
#endif


// It makes no sense to test single \p e. Consider we may be given an AST which
// has no constant-folding for an expression. Thus we can not take the value directly
// from an E_intLit.
inline bool isNonLogical0(const bitset_t &E, Expression const *e)
{
    VXCOV;
    return E.knownAllZero() && containsNoLogical(e);
}

/**
 * Return true when \p orig_e consists of literals or constant variables.
 * Follow those common cases in isXDueToLiteral() and hasLitervalX().
 **/
bool ConstantExpressionResult::recursiveIsConstOrLiteral(Expression const *orig_e)
{
    Expression const *e = strip_casts(orig_e);
    e = skipCompilerGeneratedBoolification(e);
    GDOUT (checkers, "orig: " << *orig_e << ", skip compiler boolify: " << *e);
    ASTSWITCH(e) {
        ASTCASE(E_binary, bny) {
            if(bny->op == BIN_COMMA)
                return recursiveIsConstOrLiteral(bny->e2);
            else
                return recursiveIsConstOrLiteral(bny->e1)
                    && recursiveIsConstOrLiteral(bny->e2);
        }
        ASTCASE(E_unary, uny) {
            return recursiveIsConstOrLiteral(uny->expr);
        }
        ASTCASE(E_cond, cond) {
            return recursiveIsConstOrLiteral(cond->cond)
                    && (recursiveIsConstOrLiteral(cond->th)
                        || recursiveIsConstOrLiteral(cond->el));
        }
        ASTCASE(E_cast, cast) {
            return recursiveIsConstOrLiteral(cast->expr);
        }
        ASTCASE1(E_intLit) {
            // NB: const-var, const-expr, enumerator will be folded into a
            // E_intLit. That's why this function is named as
            // isConstOrLiteral.
            return true;
        }
        ASTCASE1(E_predefinedConstantLit) {
            // return true; // TODO: uncomment this
            // This can't happen in Indio,
            // since BIN_OR is not used in JavaScript AST.
            xfailure("unexpected constant");
        }
        ASTENDCASED;
    }
    return false;
}

bool ConstantExpressionResult::isConstOrLiteral(Expression const *e)
{
    return recursiveIsConstOrLiteral(e);
}


static bool recursiveIsXDueToLiteral(Expression const *orig_e,
                                     bool              x)
{
    Expression const *e = strip_casts(orig_e);
    e = skipCompilerGeneratedBoolification(e);
    ASTSWITCH(e) {
    ASTCASE(E_binary, bny) {
        switch (bny->op) {
        case BIN_AND:
            VXCOV;
            return x ? (recursiveIsXDueToLiteral(bny->e1, x) &&
                        recursiveIsXDueToLiteral(bny->e2, x)) :
                       (recursiveIsXDueToLiteral(bny->e1, x) ||
                        recursiveIsXDueToLiteral(bny->e2, x));
            break;

        case BIN_OR:
            VXCOV;
            return x ? (recursiveIsXDueToLiteral(bny->e1, x) ||
                        recursiveIsXDueToLiteral(bny->e2, x)) :
                       (recursiveIsXDueToLiteral(bny->e1, x) &&
                        recursiveIsXDueToLiteral(bny->e2, x));
            break;

        case BIN_COMMA:
            VXCOV;
            return recursiveIsXDueToLiteral(bny->e2, x);
            break;

        default:
            ;
        }

        VXCOV;
        return false;
    }

    ASTCASE(E_unary, uny) {
        if (uny->op == UNY_NOT) {
            VXCOV;
            return recursiveIsXDueToLiteral(uny->expr, !x);
        }

        VXCOV;
        return false;
    }

    ASTCASE(E_cond, cond) {
        VXCOV;
        return (recursiveIsXDueToLiteral(cond->cond, true) &&
                recursiveIsXDueToLiteral(cond->th, x)) ||
               (recursiveIsXDueToLiteral(cond->cond, false) &&
                recursiveIsXDueToLiteral(cond->el, x));
    }

    ASTCASE(E_dup, dup) {
        VXCOV;
        return recursiveIsXDueToLiteral(dup->reused, x);
    }
    ASTENDCASED;
    }

    VXCOV;
    return hasLiteralX(e, x ? 1 : 0, /*immediate*/false, /*allowLogical*/true);
}


inline bool isXDueToLiteral(Expression const *e, bool x)
{
    VXCOV;
    return recursiveIsXDueToLiteral(e, x);
}


inline bool isFalseDueToLiteral(Expression const *e)
{
    VXCOV;
    return isXDueToLiteral(e, false);
}


inline bool isTrueDueToLiteral(Expression const *e)
{
    VXCOV;
    return isXDueToLiteral(e, true);
}


#if NOT_CURRENTLY_USED
inline bool isBooleanDueToLiteral(Expression const *e)
{
    return isFalseDueToLiteral(e) || isTrueDueToLiteral(e);
}

inline bool isJavaScriptBoolean(Expression const* e) {
    if (const E_predefinedConstantLit* pred = e->ifE_predefinedConstantLit()) {
        return pred->constant == CONSTANT_TRUE || pred->constant == CONSTANT_FALSE;
    }
    return false;
    // Old - now wrong - implementation.
    //return e->type->ifscalar_type_tC()
        //&& (e->type->ifscalar_type_tC()->scalar_kind() == types::scalars::KIND_BOOL);
}

inline bool isJavaScriptBooleanStyle(Expression const* e) {
    bool ret = isBoolification(e) ||
            (e->isE_unary() && e->ifE_unaryC()->op == UNY_NOT) ||
            (e->isE_binary() && e->ifE_binaryC()->op == BIN_DYNAMIC_EQUAL) ||
            (e->isE_binary() && e->ifE_binaryC()->op == BIN_DYNAMIC_NOTEQUAL) ||
            isJavaScriptBoolean(e);
    return ret;
}

inline bool sureOfJavascriptNonBoolness(Expression const* e) {
    bool ret = e->isE_floatLit() ||
            isJavaScriptUndefinedExpression(e) ||
            isJavaScriptNullExpression(e);
    return ret;
}
#endif


enum true_precedence_t {
  MinTruePrecedence,
  CommaTruePrecedence = MinTruePrecedence,
  AssignTruePrecedence,
  TernaryTruePrecedence,
  OrTruePrecedence,
  AndTruePrecedence,
  BitOrTruePrecedence,
  BitXorTruePrecedence,
  BitAndTruePrecedence,
  EqualTruePrecedence,
  CompareTruePrecedence,
  ShiftTruePrecedence,
  AddTruePrecedence,
  MultiplyTruePrecedence,
  CastTruePrecedence,
  UnaryTruePrecedence,
  EffectTruePrecedence,
  MaxTruePrecedence
};

static true_precedence_t getTruePrecedence(Expression const *e)
{
    ASTSWITCH(e) {
    ASTCASE1(E_assign) VCOV; return AssignTruePrecedence;

    ASTCASE1(E_cond) VCOV; return TernaryTruePrecedence;

    ASTCASE(E_binary, biny) {
        switch (biny->op) {
        case BIN_COMMA: VXCOV; return CommaTruePrecedence;

        case BIN_OR: VCOV; return OrTruePrecedence;

        case BIN_AND: VCOV; return AndTruePrecedence;

        case BIN_BITOR: VXCOV; return BitOrTruePrecedence;

        case BIN_BITXOR: VCOV; return BitXorTruePrecedence;

        case BIN_BITAND: VXCOV; return BitAndTruePrecedence;

        case BIN_EQUAL:
        case BIN_NOTEQUAL:
        case BIN_DYNAMIC_EQUAL:
        case BIN_DYNAMIC_NOTEQUAL: return EqualTruePrecedence;

        case BIN_LESS:
        case BIN_LESSEQ:
        case BIN_GREATER:
        case BIN_GREATEREQ:
        case BIN_MINIMUM:
        case BIN_MAXIMUM:
        case BIN_JAVASCRIPT_INSTANCEOF:
        case BIN_JAVASCRIPT_IN: VCOV; return CompareTruePrecedence;

        case BIN_LSHIFT:
        case BIN_URSHIFT:
        case BIN_RSHIFT: VXCOV; return ShiftTruePrecedence;

        case BIN_PLUS:
        case BIN_STRCAT:
        case BIN_DYNAMIC_PLUS:
        case BIN_MINUS: VCOV; return AddTruePrecedence;

        case BIN_MULT:
        case BIN_DIV:
        case BIN_MOD: VXCOV; return MultiplyTruePrecedence;

        case BIN_DOT_STAR: VCOV; return MaxTruePrecedence;

        case BIN_ASSIGN: VCOV; return MaxTruePrecedence; // to shut up warning
        }
    }

    ASTCASE1(E_unary) VXCOV; return UnaryTruePrecedence;
    ASTCASE1(E_new) VCOV; return UnaryTruePrecedence;
    ASTCASE1(E_delete) VCOV; return UnaryTruePrecedence;

    ASTCASE1(E_effect) VCOV; return EffectTruePrecedence;

    // Bug 57206. We were getting extra casts through from the front
    // end, and since cast is normally a very high precedence operator
    // we were assuming that we had the missing parenthesis pattern
    // (something like "!arg1 & arg2" where it is more likely that the
    // high precedence ! operator was intended to be applied to the
    // whole bitwise and expression).

    // To fix this we ignore the cast and get the precedence of
    // whatever is underneath it in the tree.

    // To justify the fix, we note that cast does not change the value
    // of the argument whereas other operators do (although as a
    // counterexample, a narrowing cast might change the value of
    // the operand - at some point we might need to deal with that).
    // See Bug 58047 for the "narrowing casts" problem.

    ASTCASE(E_cast, cast) VXCOV; return getTruePrecedence(cast->expr);

    ASTENDCASED VXCOV; return MaxTruePrecedence;
    }
}


// machinery to print deeply unfolded literals
static astenv_ASTNode E(Expression const *e)
{
    PrintOptions opts(PrintOptions::likeSource());
    opts.printBlocks = false;
    return astenv_ASTNode(*skipCompilerGeneratedBoolification(e), opts);
}


static string
hexOnIt(unsigned long long vv)
{
    VXCOV;
    ostringstream os;
    if (0 <= vv && vv <= 9) {
        os << vv;
    } else {
        using namespace std;
        os << hex << showbase << vv << dec << noshowbase;
    }
    return os.str();
}


// Is 'e' within an unreachable branch of an expression, e.g.
//    0 ? E(e) : _
//    1 ? _ : E(e)
//    0 && E(e)
// (where 'E(x)' is any expression containing 'x')?
static bool unreachableExpression(Expression const *e)
{
    BEGIN_PATTERNS;
    static Const_int const_pat;
    static Const_int zero(0);
    static DECL_PAT(non_zero, And(const_pat, Not(zero)));
    static PtrSameExpression same_as_e;
    static ExprContainsPat contains_e(same_as_e);
    static CondPattern true_cond_pat(non_zero, _, contains_e);
    static CondPattern false_cond_pat(zero, contains_e, _);
    static Binop true_or_pat(BIN_OR, non_zero, contains_e);
    static Binop false_and_pat(BIN_AND, zero, contains_e);
    END_PATTERNS;

    same_as_e.set(e);
    return within(e, Or(true_cond_pat, false_cond_pat,
                        true_or_pat, false_and_pat));
}


// Within the context of a logical and bitwise expression E, if we report a
// defect in _ we don't want to report another on E(_).  However, reporting
// a defect in e1 should not preclude reporting another defect in e2 of
// E(e1, e2).  When we're considering reporting a defect for 'e' we look to
// see if we've previously reported something for 'e' (or for any of its
// descendents).  If so, we return false.  Otherwise, we remember reporting
// 'e' and walk up to the "canonical expression root" of 'e', repeating this
// process at each step.
static bool remember(Expression const *e, set<Expression const *> *reported)
{
    VXCOV;
    while (true) {
        if (contains(*reported, e)) {
            VXCOV;
            return false;
        }
        //cout << "remembering " << e->kindName() << " " << (short *)e << " " << *e << endl;
        //if (e->isE_binary()) {cout << "  bin-op: " << e->asE_binaryC()->op << endl;}
        reported->insert(e);

        Expression const *parentExpr = e->ifExpressionParent();
        if (parentExpr == NULL) {
            //cout << "  non-expr parent " << (short *)e->parent << " " << *e->parent << endl;
            VXCOV;
            return true;
        }
        ASTSWITCH(parentExpr) {
        ASTCASE(E_binary, biny) {
            switch (biny->op) {
            case BIN_COMMA:
                if (biny->e1 == e) {
                    VXCOV;
                    return true;
                }
                VXCOV;
                break;

            case BIN_LSHIFT:
                VXCOV;
            case BIN_URSHIFT:
                VXCOV;
            case BIN_RSHIFT:
                if (biny->e2 == e) {
                    VXCOV;
                    return true;
                }
                VXCOV;
                break;

            default:
                ;
            }
        }
        ASTCASE1(E_funCall)
            VXCOV;
            return true;
        ASTCASE1(E_cond)
            VXCOV;
            return true;
        ASTCASE1(E_subscript)
            VXCOV;
            return true;
        ASTCASE1(E_mapAcc)
            VXCOV;
            return true;
        ASTCASE1(E_new)
            VXCOV;
            return true;
        ASTCASE1(E_delete)
            VXCOV;
            return true;
        ASTCASE1(E_temp_init)
            VCOV;
            return true;
        ASTCASE(E_dup, dup) {
            VCOV;
            if (contains(*reported, dup->reused)) {
                VXCOV;
                return false;
            }
            return true;
        }
        ASTENDCASED;
        }

        VXCOV;
        e = parentExpr;
    }
}


err_t *ConstantExpressionResult::createErrorWithEvent(const char   *tag,
                                                      const char   *subcategory,
                                                      string const &msg)
{
    if (unreachableExpression(curTree)) {
        VCOV;
        return NULL;
    }

    if (!remember(curTree, reported)) {
        VXCOV;
        return NULL;
    }

    VXCOV;
    err_t *error = create_nopath_error(*curTraversal);
    error->add_event(tag, msg, curTree);
    error->set_subcategory(subcategory);
    return error;
}


err_t *ConstantExpressionResult::createErrorWithEventAndElaboration(
    const char   *tag,
    const char   *subcategory,
    string const &msg,
    string const &elaboration)
{
    if (unreachableExpression(curTree)) {
        VCOV;
        return NULL;
    }

    if (!remember(curTree, reported)) {
        VXCOV;
        return NULL;
    }

    VXCOV;
    err_t *error = create_nopath_error(*curTraversal);
    error->add_event(tag, msg, curTree);
    error->set_main_event();
    if (!elaboration.empty()) {
        error->add_remediation_event("remediation", elaboration, curTree);
    }
    error->set_subcategory(subcategory);
    return error;
}


err_t *ConstantExpressionResult::createErrorWithEventAndContext(
    const char       *tag,
    const char       *subcategory,
    string const     &msg,
    Expression const *expr_for_context)
{
    if (unreachableExpression(curTree)) {
        VCOV;
        return NULL;
    }

    if (!remember(curTree, reported)) {
        VXCOV;
        return NULL;
    }

    VXCOV;
    err_t *error = create_nopath_error(*curTraversal);
    error->add_event(tag,
        concatCovLStrv2(msg, exprContext(expr_for_context)), curTree);
    error->set_subcategory(subcategory);
    return error;
}


err_t *ConstantExpressionResult::createErrorWithEventAndContextAndElaboration(
    const char       *tag,
    const char       *subcategory,
    string const     &msg,
    Expression const *expr_for_context,
    string const      &elaboration)
{
    if (unreachableExpression(curTree)) {
        VCOV;
        return NULL;
    }

    if (!remember(curTree, reported)) {
        VCOV;
        return NULL;
    }

    VXCOV;
    err_t *error = create_nopath_error(*curTraversal);
    error->add_event(tag,
        concatCovLStrv2(msg, exprContext(expr_for_context)),
        curTree);
    error->set_main_event();
    if (!elaboration.empty()) {
        error->add_remediation_event("remediation", elaboration, curTree);
    }
    error->set_subcategory(subcategory);
    return error;
}


err_t *ConstantExpressionResult::createErrorWithEventAndContextAndElaborations(
    const char       *tag,
    const char       *subcategory,
    string const     &msg,
    Expression const *expr_for_context,
    string const      &elaboration,
    string const      &elaboration2)
{
    if (unreachableExpression(curTree)) {
        VXCOV;
        return NULL;
    }

    if (!remember(curTree, reported)) {
        VXCOV;
        return NULL;
    }

    VXCOV;
    err_t *error = create_nopath_error(*curTraversal);
    error->add_event(tag,
        concatCovLStrv2(msg, exprContext(expr_for_context)),
        curTree);
    error->set_main_event();
    if (!elaboration.empty()) {
        error->add_remediation_event("remediation",
                    concatCovLStrv2(elaboration, elaboration2),
                    curTree);
        }
    error->set_subcategory(subcategory);
    return error;
}


void ConstantExpressionResult::rememberReportingError()
{
    VXCOV;
    (void)remember(curTree, reported);
}


struct visit_arg_t {
    ConstantExpressionResult *checker;
};


static bool LVBCVisitor(void *_arg, ASTNode *t)
{
    VXCOV;
    visit_arg_t *arg = (visit_arg_t *)_arg;
    arg->checker->my_handle_tree(t);
    return true;
}


// Is the current function an instantiation of a template taking a non-type
// template parameter?
static bool check_for_integral_template_argument(char const *fn_name)
{
     using namespace unmangleAST;
     Name const *n = getUnmangledNameWithoutArgs(fn_name);
     if (n == NULL) {
         return false;
     }
     VectorA<TemplateArg const *> templateArgs(new_delete_allocator);
     n->getTemplateArgs(templateArgs);
     foreach(i, templateArgs) {
         if((*i)->isExpressionTemplateArg()) {
             return true;
         }
     }
     return false;

}

void ConstantExpressionResult::init_fn(state_t           *state,
                                       abstract_interp_t &cur_traversal)
{
    VXCOV;
    // capture arguments so we don't need to pass them all over the
    // place

    // XXX CHG: That's equivalent to using global variables, which as
    // we all know are evil.
    arena = state->get_arena();
    curTraversal = &cur_traversal;
    curLang = curTraversal->getLang();
}

void ConstantExpressionResult::final_fn(abstract_interp_t &cur_traversal)
{
    VXCOV;
    set<const Expression *> reportedSet;
    reported = &reportedSet;

    visit_arg_t visitor = {this};
    curTraversal->fn_trav().get_AST_fn()->visitTree(
        LVBCVisitor, &visitor,
        (ASTNode::VISIT_ALL  & ~ASTNode::VISIT_GENERATED_CODE) |
            ASTNode::VISIT_POSTORDER);
    reported = NULL;

    checker_t::final_fn(cur_traversal);
}


#if 0
// just keeping this for future reference
err_t *ConstantExpressionResult::diagnoseLoneBangInBitOr(
    Expression const *e,
    ASTNode const    *context)
{
    // on the lhs, !_ | <bits> might have been meant to be
    // "!_ || <bits>" or "!(_ | <bits>)" or "~_ | bits" (any other guesses?)
    // try to intuit which

    // on the rhs, <bits> | !_  might have been meant to be
    // "<bits> || !_" or "bits | ~_" (any other guesses?)
    // try to intuit which

    Expression const *bang_operand = e->asE_unaryC()->expr;
    bit_details_t bang_operand_bd = getBitDetails(bang_operand, 0);
    if (bitsNotLogical(bang_operand_bd.exprClasses)) {
        return createErrorWithEvent(
            "logical_in_bits_context",
            desc() << /*"#2 " <<*/ E(e) << " is logical in the bits context "
                   << *context << " (" << E(bang_operand)
                   << ", the operand of !, is itself bits, suggesting that you "
                   << "might have intended ~ rather than !).");
    } else if (!contains(bang_operand_bd.exprClasses, BitsExprClass)) {
        return createErrorWithEvent(
            "logical_in_bits_context",
            desc() << /*"#3 " <<*/ E(e) << " is logical in bits context " << *context
                   << " Did you intend to use '||' rather than '|'?");
    } else {
        return createErrorWithEvent(
            "bits_vs_logical_consistency",
            desc() << *context << " mixes seemingly logical and seemingly "
                   << "bitwise operands with the bit '|' operator in a "
                   << "questionable manner");
    }
}
#endif



struct constant_eval_t {
    static bitset_t eval(Expression const *e, ConstantExpressionResult *sm)
    {
        VXCOV;
#if CER_USE_STORE
        bit_store_t *store = static_cast<bit_store_t *>(sm->get_state());
        bit_store_t::iterator p = store->find(e);
        if(p != store->end()) {
            //cout << "pulled " << p->second << " from store for " << *e << endl;
            return p->second;
        }
#endif

        Integer integer;
        if (integer.match(e)) {
            if (integer.get_type()->get_type_size() > sizeof(uint64)) {
                // a larger integer type than we can handle
                VXCOV;
                itype_parms type_parms(scalar_itype_parms(integer.get_type()));
                return bitset_t::specificBottom(type_parms, NULL /*expr*/,
                                                false /*!allow_too_big_integers*/);
            }
            VXCOV;
        }
        bitset_t result = bitset_t::bottomForExpr(e);
        //cout << e->kindName() << " ==> " << result << endl;
        return result;
    }

    static bitset_t eval(E_subscript const *subscript, ConstantExpressionResult *)
    {
        VCOV;
        return bitset_t::bottomForExpr(subscript);
    }

    static bitset_t eval(E_mapAcc const *acc, ConstantExpressionResult *)
    {
        VCOV;
        if(isJavaScriptUndefinedExpression(acc)) {
            return bitset_t::fromValue(0, itype_parms(64, true), NULL);
        }
        return bitset_t::bottomForExpr(acc);
    }

    static bitset_t eval(E_variable const *var, ConstantExpressionResult *sm)
    {
        VXCOV;
#if REAL_SOON_NOW
        // if 'var' is a C++ constant, get its value
#else
        return eval((Expression const *)var, sm);
#endif
    }

#if SIZEOF_ENCOUNTERED_IN_TREE_TRAVERSAL
    // Although a sizeof expression is clearly a constant for language-semantics
    // purposes, it is best for C_E_R's purposes to treat it as non-constant
    // because it can vary across builds.
    static bitset_t eval(E_sizeof const *szof, ConstantExpressionResult *)
    {
        VCOV;
        //cout << "sizeof(expr) ==> unsignedBottom" << endl;
        return bitset_t::unsignedBottom(szof);
    }

    // Ditto.
    static bitset_t eval(E_sizeofType const *szof, ConstantExpressionResult *)
    {
        VCOV;
        //cout << "sizeof(type) ==> unsignedBottom" << endl;
        return bitset_t::unsignedBottom(szof);
    }
#endif

    static bitset_t eval(E_floatLit const *floatLit, ConstantExpressionResult *sm)
    {
        if(isJavaScript(sm->curTraversal->getLang()) &&
            floatLit->original_expr != NULL &&
            floatLit->original_expr->isE_intLit()) {
                return eval((const E_intLit*)floatLit->original_expr, sm);
        }
        return eval((const Expression * ) floatLit, sm);
    }

    static bitset_t eval(E_intLit const *intLit, ConstantExpressionResult *sm)
    {
        BEGIN_PATTERNS;
        static SizeofPattern sizeof_pattern;
        static ContainsPat contains_sizeof(sizeof_pattern);
        static Integer integer;
        static Bool boolean;
        END_PATTERNS;

        if (integer.match(intLit)) {
            scalar_type_t const *type = integer.get_type();
            itype_parms type_parms(scalar_itype_parms(type));
            if (intLit->original_expr) {
                // See two evals(), above.
                if (contains_sizeof.match(intLit->original_expr)) {
                    //cout << "sizeof ==> bottom" << endl;
                    VXCOV;
                    return bitset_t::specificBottom(type_parms, NULL /*expr*/,
                                                    false /*!allow_too_big_integers*/);
                }

                // Ideally we'd like to know for certain which "constants"
                // can vary across builds, but lacking that knowledge we
                // use the heuristic that if they bothered to put
                // false/true (C++/Java) or 0/1 (C++) into a  constant that's
                // probably because it is defined differently in different
                // configurations.  In those cases we'll back off and say
                // we know it's a bool (or, at least, can only be 0 or 1),
                // but not its specific value.
                Expression const *orig = strip_casts(intLit->original_expr),
                           *oe = skipCompilerGeneratedBoolification(orig);
                // Consider we may be given an AST that has an extra constant-folding
                // on the global variable.
                if (oe != orig) {
                    oe = extractFoldedExpr(oe);
                }

                GDOUT (checkers, *intLit << " => " << orig << " => " << *oe);

                if (oe->isE_variable() &&
                    (boolean.match(oe) ||
                     (isCLike(sm->curTraversal->getLang()) &&
                      integer.match(oe) &&
                      (intLit->i == 0 || intLit->i == 1)))) {
                    GDOUT (checkers, "bool or bool-ish constant ==> unknown bool");
                    VXCOV;
                    return bitset_t::booleanBottom(type_parms, intLit);
                } else {
                    VXCOV;
                }
            } else {
                VXCOV;
            }
            if (type->get_type_size() > sizeof(intLit->i)) {
                // a larger integer type than we can handle
                VXCOV;
                return bitset_t::specificBottom(type_parms, NULL /*expr*/,
                                                false /*!allow_too_big_integers*/);
            }

            bitset_t t = bitset_t::fromValue(intLit->i, type_parms, intLit);
            return t;
        }

        VXCOV;
        // could be a constant address
        //cout << "intLit of type " << *intLit->type << " ==> bottom" << endl;
        return bitset_t::nonIntegralBottom();
    }

    static bitset_t eval(E_predefinedConstantLit const *lit, ConstantExpressionResult *sm)
    {
        switch( lit->constant ) {
        case CONSTANT_UNDEFINED:
        case CONSTANT_NULL:
            return bitset_t::fromValue(0, itype_parms(64, true), NULL);
        case CONSTANT_FALSE:
            return bitset_t::fromValue(0, itype_parms(1, true), NULL);
        case CONSTANT_TRUE:
            return bitset_t::fromValue(1, itype_parms(1, true), NULL);
        }

        VXCOV;
        return bitset_t::nonIntegralBottom();
    }

    static bitset_t evalDotStar(E_binary const *e, ConstantExpressionResult *sm)
    {
        return eval((Expression const *)e, sm);
    }

    static bitset_t evalJavaScriptInstanceof(E_binary const *e, ConstantExpressionResult *state)
    {
        return eval((Expression const *)e, state);
    }

    static bitset_t evalJavaScriptIn(E_binary const *e, ConstantExpressionResult *state)
    {
        return eval((Expression const *)e, state);
    }
};


// C++ won't let you specialize a template in a different namespace, so we have
// to reenter the expr_eval namespace for these.
OPEN_NAMESPACE(expr_eval);

template<>
bitset_t evalFunCall<bitset_t, bitset_t, ConstantExpressionResult>(
    E_funCall const *funCall, ConstantExpressionResult *sm)
{
    VXCOV;
    return constant_eval_t::eval((Expression const *)funCall, sm);
}


template<>
bitset_t evalPtrDiff<bitset_t, ConstantExpressionResult, null_filter,
    constant_eval_t>(Expression const *diff,
                     Expression const *e1,
                     Expression const *e2,
                     ConstantExpressionResult *,
                     TU_LANG lang)
{
    VXCOV;
    return bitset_t::bottomForExpr(diff);
}

CLOSE_NAMESPACE(expr_eval);


bitset_t ConstantExpressionResult::eval(Expression const *e)
{
    VXCOV;
    return expr_eval::eval<bitset_t,
                           ConstantExpressionResult,
                           /*strip_casts*/expr_eval::null_filter,
                           constant_eval_t>(e, this, curTraversal->getLang());
}


// Named after the arcnet package in the linux kernel, within which appears
// one of the construct that this hack detects: "FLAGSET & arcnet_debug & FLAG".
// FLAGSET & FLAG may be zero, but because of the way this construct is written,
// with the variable between the two constants, no constant folding occurs.
// Consequently, our normal special-casing of bitwise-and with zero doesn't
// kick in.  Look for the ((c1 & v) & c2) tree and, if found, evaluate c1 & c2.
// Return true if the pattern is found and c1 & c2 == 0.
// Another similar case is "_ & FLAGSET & FLAG", where the tree is ((_ &
// FLAGSET) & FLAG), such that, again, we don't see that (FLAGSET&FLAG)==0.
static bool arcnetHack(Expression const *e1, Expression const *e2)
{
    if (!e1->isBinary(BIN_BITAND)) {
        VXCOV;
        return false;
    }

    E_binary const *bny = static_cast<E_binary const *>(e1);
    E_intLit const *c2 = e2->ifE_intLitC();
    if (c2 == NULL) {
        VXCOV;
        return false;
    }

    E_intLit const *burriedLit = bny->e1->ifE_intLitC();
    if (burriedLit == NULL) {
        VXCOV;
        burriedLit = bny->e2->ifE_intLitC();
    } else {
        VXCOV;
    }

    if (burriedLit == NULL) {
        VXCOV;
        return false;
    }

    if ((burriedLit->i & c2->i) == 0) {
        VXCOV;
        return true;
    }

    VXCOV;
    return false;
}


// What, exactly, constitutes plain vanilla "1" or "true" is somewhat
// arbitrary, and subject to tuning, but this is a simple and straightforward
// interpretation.
static bool is1OrTrue(Expression const *e)
{
    if (E_intLit const *intLit = e->ifE_intLitC()) {
        if (intLit->original_expr) {
            VXCOV;
            return is1OrTrue(intLit->original_expr);
        }
        if (scalar_type_t const *type = e->type->as_scalar_p()) {
            if (type->scalar_kind() == types::scalars::KIND_BOOL) {
                if (intLit->i != 0) {
                    VXCOV;
                    return true;
                }
                VCOV;
            } else {
                VXCOV;
            }
        } else {
            VCOV;
        }
        if (intLit->i == 1) {
            VXCOV;
            return true;
        }
        VXCOV;
    } else if (E_binary const *bny = e->ifE_binaryC()) {
        if (bny->op == BIN_NOTEQUAL && bny->isImplicit == PI_IMPLICIT &&
                   isLiteral1(bny->e1)) {
            VXCOV;
            return true;
        }
        VXCOV;
    } else {
        VXCOV;
    }

    return false;
}


// What, exactly, constittutes plain vanilla "0" or "false" is somewhat
// arbitrary, and subject to tuning, but this is a simple and straightforward
// interpretation.
static bool is0OrFalse(Expression const *e)
{
    if (E_intLit const *intLit = e->ifE_intLitC()) {
        if (intLit->original_expr) {
            VXCOV;
            return is0OrFalse(intLit->original_expr);
        }
        if (scalar_type_t const *type = e->type->as_scalar_p()) {
            if (type->scalar_kind() == types::scalars::KIND_BOOL) {
                if (intLit->i == 0) {
                    VXCOV;
                    return true;
                }
                VCOV;
            } else {
                VXCOV;
            }
        }
        if (intLit->i == 0) {
            VXCOV;
            return true;
        }
        VXCOV;
    } else if (E_binary const *bny = e->ifE_binaryC()) {
        if (bny->op == BIN_NOTEQUAL && bny->isImplicit == PI_IMPLICIT &&
                isLiteral0(bny->e1)) {
            VXCOV;
            return true;
        }
        VXCOV;
    } else {
        VXCOV;
    }

    return false;
}


// Return a complete sentence describing the non-cast context in which 'e'
// occurs.
string ConstantExpressionResult::exprContext(Expression const *e)
{
    event_stream &es = desc() << " This occurs ";
// These otherwise-unconscionable macros are used so that all of the string
// literal constituents of the text put into 'es' are visible to the intRA-
// procedural analysis done by the defect message extraction tool used as part
// of our i18n process.  On any given path it needs to see nothing but string
// literals being <<'d into an event_stream which eventually flows into a
// call to consume_event_stream().  CES handles that for the simple case of a
// single string literal.
#   define CES_NOPAREN(s) \
    consume_event_stream(es << "as " << s << ".")
#   define CES(s) CES_NOPAREN((s))

    Expression const *child = e;
    ASTNode const *parent = parentSkippingIntLit(child);
    if (parent != child->parent) {
        child = static_cast<Expression const *>(child->parent);
    }
    sup_assert(child->parent == parent);
    bool isBool = false;
    scalar_type_t const *scalar_type = e->type->as_scalar_p();
    if (scalar_type != NULL &&
        scalar_type->scalar_kind() == types::scalars::KIND_BOOL) {
        VXCOV;
        isBool = true;
    } else {
        VXCOV;
    }
    bool converting = false;
    if (inCompilerGeneratedBoolificationContext(child)) {
        VXCOV;
        child = parent->asExpressionC();
        parent = parentSkippingIntLit(child);
        converting = true;

        // One might think that an already-bool expression would never have
        // compiler-generated Boolification applied to it, but as bz 30403
        // demonstrates that's not reliably true.  When an already-bool
        // expression is explicitly cast to bool we get a somewhat "surprising"
        // AST.

        // assert(!isBool);

        // Even though this is semantically a conversion to bool, only *say*
        // "bool" for C++ (i.e., if that's actually what the new type is).
        isBool = child->type->as_scalar_p() &&
                 child->type->as_scalar_p()->scalar_kind() ==
                     types::scalars::KIND_BOOL;
    } else {
        VXCOV;
    }
    if (Expression const *exprParent = parent->ifExpressionC()) {
        parent = expr_strip_up(exprParent);
        if (parent != exprParent) {
            child = find_outermost_cast(exprParent);
            cond_assert(child->parent == parent);
            if (scalar_type_t const *type = child->type->as_scalar_p()) {
                if (type->scalar_kind() == types::scalars::KIND_BOOL) {
                    VXCOV;
                    isBool = true;
                } else {
                    VXCOV;
                }
            } else {
                VXCOV;
            }
        } else {
            VXCOV;
        }
    } else {
        VXCOV;
    }
// CESHELPER handles the common case where one further string would be supplied
// to HELPER.  It works like CES except that it invokes HELPER first.
#   define CESHELPER(remainder) \
consume_event_stream(es << (converting ? \
  "when converting to bool for " : "as ") << remainder << ".")

    ASTNODE_SWITCH(parent) {
        ASTNODE_CASE(Statement, sParent) {
        ASTSWITCH(sParent) {
        ASTCASE1(S_if) {
            VXCOV;
            return CES("the logical operand of if");
        }
        ASTCASE1(S_while) {
            VXCOV;
            return CES("the logical operand of while");
        }
        ASTCASE1(S_doWhile) {
            VXCOV;
            return CES("the logical operand of do-while");
        }
        ASTCASE1(S_for) {
            VXCOV;
            return CES("the logical operand of for");
        }
        ASTCASE1(S_return) {
            VXCOV;
            return CESHELPER("a return value");
        }
        ASTENDCASED;
        }
        VXCOV;
    }
        ASTNODE_CASE(Expression, eParent) {
        ASTSWITCH(eParent) {
        ASTCASE(E_binary, biny) {
            sup_assert(biny->e1 == child || biny->e2 == child);
            bool first = biny->e1 == child;
            switch (biny->op) {
            case BIN_AND:
                VXCOV;
                return CES(first ? "the logical first operand of '&&'" :
                                   "the logical second operand of '&&'");
                break;

            case BIN_OR:
                VXCOV;
                return CES(first ? "the logical first operand of '||'" :
                                   "the logical second operand of '||'");
                break;

            case BIN_BITAND:
                VXCOV;
                return CES(first ? "the bitwise first operand of '&'" :
                                   "the bitwise second operand of '&'");
                break;

            case BIN_BITOR:
                VXCOV;
                return CES(first ? "the bitwise first operand of '|'" :
                                   "the bitwise second operand of '|'");
                break;

            case BIN_BITXOR:
                VXCOV;
                return CES(first ? "the bitwise first operand of '^'" :
                                   "the bitwise second operand of '^'");
                break;

            default:
                ;
            }
            VXCOV;
        }
        ASTCASE(E_assign, asgn) {
            switch (asgn->op) {
            case BIN_BITAND:
                VXCOV;
                return CES("the bitwise operand of '&='");
                break;

            case BIN_BITOR:
                VXCOV; // bug in rhs of |=
                return CES("the bitwise operand of '|='");
                break;

            case BIN_BITXOR:
                VXCOV;
                return CES("the bitwise operand of '^='");
                break;

            default:
                VXCOV;
                return CESHELPER("the operand of assignment");
                break;
            }
        }
        ASTCASE(E_unary, uny) {
            switch (uny->op) {
            case UNY_NOT:
                VXCOV;
                return CES("the logical operand of '!'");
                break;

            case UNY_BITNOT:
                VXCOV;
                return CES("the bitwise operand of '~'");
                break;

            default:
                ;
            }
            VXCOV;
        }
        ASTCASE1(E_funCall) {
            VXCOV;
            return CESHELPER("an argument to a function call");
        }
        ASTCASE(E_cond, cond) {
            if (cond->condKind == CEK_JAVASCRIPT_AND) {
                bool first = cond->cond == child || cond->th == child;
                return CES(first ? "the logical first operand of '&&'" :
                               "the logical second operand of '&&'");
            } else if (cond->condKind == CEK_JAVASCRIPT_OR) {
                bool first = cond->cond == child || cond->th == child;
                return CES(first ? "the logical first operand of '||'" :
                               "the logical second operand of '||'");
            } else if (cond->cond == child
                ||
                (cond->condKind != CEK_TERNARY && cond->th == child)
                ||
                (cond->condKind == CEK_TERNARY && isJavaScript(curTraversal->getLang()) && strip_casts(cond->cond) == skipCompilerGeneratedBoolification(child))
            ) {
                VXCOV;
                // Wrap the alternative in a string to factor it out
                // of the translatable part.
                return CES_NOPAREN("the logical first operand of '"
                           << string(cond->isNullCoalescing() ? "??":"?:")
                           << "'");
            } else {
                VXCOV;
                sup_assert(cond->th == child || cond->el == child);
                return CESHELPER("the " <<
                                 ((cond->condKind != CEK_TERNARY || (cond->th == child)) ? "second" : "third") <<
                                     " operand of '"
                                 << string(cond->isNullCoalescing() ? "??":"?:") << "'");
            }
        }
        ASTENDCASED;
        }
        VXCOV;
    }
        ASTNODE_CASE1(Initializer) {
            VXCOV;
            return CESHELPER("an initializer");
        }
        ASTDEFAULT {
            // The parent of an expression can't be Declaration or
            // Function (except for a Declaration's varExpr, but we
            // shouldn't see one of these here)
            xfailure("Bad AST parent");
        }
        ASTNODE_ENDCASE;
    }

    if (parent->isStatement() && e->isE_assign()) {
        switch (e->asE_assignC()->op) {
        case BIN_BITAND:
            VXCOV;
            return CES("the value assigned by '&='");
            break;

        case BIN_BITOR:
            VXCOV;
            return CES("the value assigned by '|='");
            break;

        case BIN_BITXOR:
            // This is basically impossible to exercise until value tracking
            // allows us to know something about the lhs.
            VCOV;
            return CES("the value assigned by '^='");
            break;

        default:
            sup_abort("unexpected compound assignment");
        }
    } else {
        VXCOV;
    }
    if (isBool) {
      //GCC UPGRADE 4.6.3
      //Unused variable warning = error
    }

    return CESHELPER("a value");
#   undef CES
#   undef CESHELPER
}


// Are the bits in 'a', going right-to-left, a prefix of the bits in 'b'?
static bool is_prefix_bitvector(bitype a, bitype b)
{
    int const highestBit = 8 * sizeof(bitype) - 1;
    bitype const hibit = 1ULL << highestBit;
    int i;
    // start by skipping any leading zeroes
    for (i = 0; i <= highestBit && (b & hibit) == 0; ++i, a <<= 1, b <<= 1) {
        VXCOV;
    }

    // skip all of the matching ones
    for (i = 0;
         i <= highestBit && (a & hibit) != 0 && (b & hibit) != 0;
         ++i, a <<= 1, b <<= 1) {
        VXCOV;
    }
    if (i == 0 || (i < highestBit && (a & hibit) != 0 && (b & hibit) == 0)) {
        VXCOV;
        // ran out of 'b'
        return false;
    }
    VXCOV;
    return true;
}


bitset_t redo(UnaryOp op, bitset_t const &E)
{
    switch (op) {
    case UNY_MINUS:  VCOV; return -E;
    case UNY_PLUS:   VCOV; return +E;
    case UNY_NOT:    VXCOV; return !E;
    case UNY_BITNOT: VCOV; return ~E;

    default:
        sup_abort("trying to redo unexpected unary operator");
        return bitset_t::specificBottom(E.typeParms, NULL /*expr*/,
                                        false /*!allow_too_big_integers*/);
    }
}

static bitset_t evalShift(
    BinaryOp op,
    bitset_t const &E1,
    bitset_t const &E2,
    TU_LANG lang)
{
    // Javascript shift is similar to Java shift
    if(isJava(lang) || isJavaScript(lang)) {
        VXCOV;
        return E1.javaShifted(E2, op);
    } else {
        switch(op) {
        case BIN_LSHIFT:
            VXCOV; return E1 << E2;
        case BIN_RSHIFT:
            VXCOV; return E1 >> E2;
        default:
            xfailure("Invalid non-java shift");
        }
    }
}

// Literals involving sizeof and certain kinds of constants will evaluate to
// unknown bitset_t-s, so double check for literals.
static bool value_really_not_known(bitset_t const &v, Expression const *e)
{
    VXCOV;
    return !v.valueKnown() && !e->isE_intLit()
        && !e->isE_predefinedConstantLit();
}

bitset_t ConstantExpressionResult::redo(
    BinaryOp op,
    bitset_t const &E1,
    bitset_t const &E2)
{
    switch (op) {
    case BIN_EQUAL:      VCOV; return E1 == E2;
    case BIN_NOTEQUAL:   VXCOV; return E1 != E2;
    case BIN_DYNAMIC_EQUAL:      VCOV; return E1 == E2;
    case BIN_DYNAMIC_NOTEQUAL:   VXCOV; return E1 != E2;
    case BIN_LESS:       VCOV; return E1 < E2;
    case BIN_GREATER:    VCOV; return E1 > E2;
    case BIN_LESSEQ:     VCOV; return E1 <= E2;
    case BIN_GREATEREQ:  VXCOV; return E1 >= E2;

    case BIN_MULT:       VXCOV; return E1 * E2;
    case BIN_DIV:        VCOV; return E1 / E2;
    case BIN_MOD:        VCOV; return E1 % E2;
    case BIN_PLUS:       VCOV; return E1 + E2;
    case BIN_MINUS:      VCOV; return E1 - E2;
    case BIN_LSHIFT:
    case BIN_URSHIFT:
    case BIN_RSHIFT:
        return evalShift(op, E1, E2, curTraversal->getLang());
    case BIN_BITAND:     VXCOV; return E1 & E2;
    case BIN_BITXOR:     VCOV; return E1 | E2;
    case BIN_BITOR:      VXCOV; return E1 ^ E2;
    case BIN_AND:        VCOV; return E1 && E2;
    case BIN_OR:         VCOV; return E1 || E2;
    case BIN_COMMA:      VXCOV; return E2;

    // these are never used:
    case BIN_MINIMUM:
    case BIN_MAXIMUM:
    case BIN_ASSIGN:
    case BIN_DOT_STAR:
    default:
        sup_abort("trying to redo unexpected binary operator");
        return bitset_t::specificBottom(E1.typeParms, NULL /*expr*/,
                                        false /*!allow_too_big_integers*/);
    }
}

inline bool checkRedo(bitset_t const &E, bitset_t const &RESULT)
{
    VXCOV;
    return E.valueKnown() || !RESULT.valueKnown();
}

inline bool checkRedo(bitset_t const &E1, bitset_t const &E2, bitset_t const &RESULT)
{
    VXCOV;
    return (E1.valueKnown() && E2.valueKnown()) || !RESULT.valueKnown();
}

static void typeofCaseCheck(const char *check,
                            const string &s,
                            string &closeTo,
                            string &exactMatch)
{
    const string checkStr = check;
    if (str_equal(s, checkStr)) {
        exactMatch = checkStr;
    } else if (ihas_substring(s, checkStr)) {
        closeTo = checkStr;
    } else if (s.length() >= 2 && begins_withi(checkStr, s)) {
        closeTo = checkStr;
    }
}

static E_stringLit * stringLitFromUtf8(const string &str,
                                       const type_t * type,
                                       arena_t & ar) {
    // XXX: copy-paste from regex-confusion.cpp
    vector<uint32> utf32;
    Encoding::getUTF8Encoding().multiByteToUTF32(utf32, str);
    VectorBase<char> *data = utf32_vec_to_utf32_VectorBase(utf32, ar);
    return new (ar) E_stringLit(
        EmitSourceLoc::unknownRegion,
        type,
        data);
}

err_t *ConstantExpressionResult::checkJavaScriptTypeofComparison
(Expression const *expr,
 E_javascriptTypeof const *typof,
 Expression const *otherSide,
 BinaryOp op,
 abstract_interp_t *cur_traversal)
{
    CDOUT("Checking JavaScript typeof comparison");
    // TODO: fit this better into general C_E_R.  Right now it's essentially
    // another checker buried in this one.
    err_t *error = NULL;
    ASTSWITCH(otherSide) {
    ASTCASE(E_mapAcc, ma) {
        // Check for comparison against built-in type constructors
        if (ma->notation != MAP_ACCESS_NOTATION_GLOBAL_PROPERTY) {
            break;
        }
        // else
        if (const E_stringLit * keyExpr = ma->key->ifE_stringLitC()) {
            string key = keyExpr->outNullEncUTF8();
            if (str_equal(key, "Object")) {
                goto non_string;
            } else if (str_equal(key, "Function")) {
                goto non_string;
            } else if (str_equal(key, "String")) {
                goto non_string;
            } else if (str_equal(key, "Number")) {
                goto non_string;
            } else if (str_equal(key, "Boolean")) {
                goto non_string;
            } else if (str_equal(key, "undefined")) {
                goto non_string;
            }
        }
    }
    ASTCASE1(E_floatLit) // incl integer-like
    ASTADDCASE(E_intLit) { // not actually applicable to JavaScript, but for completeness
        goto non_string;
    }
    ASTCASE1(E_new) { // incl object and array literals
        goto non_string;
    }
    ASTCASE1(E_void) {
        goto non_string;
    }
    ASTCASE1(E_predefinedConstantLit) {
    non_string:
        CDOUT("Non-string value");
        // XXX: some objects (from E_new) might convert to recognized
        // strings, but that seems unlikely in this context
        // CLUES:
        // Always false: typeof x == 0
        // Always false: typeof x > 0
        // Always false: typeof x <= 0
        // Always true:  typeof x != 0
        bool result = op == BIN_NOTEQUAL || op == BIN_DYNAMIC_NOTEQUAL;
        error =
            createErrorWithEvent(
                "typeof_misuse",
                "typeof_misuse",
                MESSAGE(expr << " is always "
                        << createCode(result ? "true" : "false")
                        << " because typeof operations always evaluate "
                        << "to a non-empty string, not " << otherSide));
    }
    ASTCASE(E_stringLit, slit) {
        bool result;
        if (op == BIN_NOTEQUAL || op == BIN_DYNAMIC_NOTEQUAL) {
            result = true;
        } else if (op == BIN_EQUAL || op == BIN_DYNAMIC_EQUAL) {
            result = false;
        } else {
            // Very weird, but OK (JavaScript does string order inequalities)
            CDOUT("Non-equality-based comparison");
            break;
        }
        string s = slit->outNullEncUTF8();
        string closeTo;
        string exactMatch;
        // There are 6 valid return values in ECMAScript 5
        typeofCaseCheck("object", s, closeTo, exactMatch);
        typeofCaseCheck("function", s, closeTo, exactMatch);
        typeofCaseCheck("string", s, closeTo, exactMatch);
        typeofCaseCheck("number", s, closeTo, exactMatch);
        typeofCaseCheck("boolean", s, closeTo, exactMatch);
        typeofCaseCheck("undefined", s, closeTo, exactMatch);
        // And 1 more in ECMAScript 6
        typeofCaseCheck("symbol", s, closeTo, exactMatch);

        if (!exactMatch.empty()) {
            CDOUT("Compared against known good value");
            break;
        } else if (!closeTo.empty()) {
            CDOUT("Close to known good string value");
            error =
                createErrorWithEventAndElaboration(
                    "typeof_misuse",
                    "typeof_misuse",
                    MESSAGE(expr << " is always "
                            << createCode(result ? "true" : "false")
                            << " except in JavaScript implementations that"
                            << " add " << slit << " to the standard set of"
                            << " possible values for typeof."),
                    MESSAGE("Did you intend to compare to "
                            << stringLitFromUtf8(closeTo, slit->type, *cur_traversal->get_arena())
                            << "?"));
        } else if (s.empty()) {
            CDOUT("Empty string value");
            error =
                createErrorWithEvent(
                    "typeof_misuse",
                    "typeof_misuse",
                    MESSAGE(expr << " is always "
                            << createCode(result ? "true" : "false")
                            << " because typeof operations never evaluate "
                            << "to the empty string."));
        } else {
            CDOUT("Bad string value");
            error =
                createErrorWithEvent(
                    "typeof_misuse",
                    "typeof_misuse",
                    MESSAGE(expr << " is always "
                            << createCode(result ? "true" : "false")
                            << " except in JavaScript implementations that"
                            << " add " << slit << " to the standard set of"
                            << " possible values for typeof."));
        }
    }
    ASTENDCASED;
        VXCOV;
    }
    return error;
}

err_t *ConstantExpressionResult::checkBitwise(Expression const *expr,
                                              Expression const *e1,
                                              Expression const *e2,
                                              BinaryOp          op)
{
    err_t *error = NULL;
    bitset_t E1 = eval(e1);
    bitset_t E2 = eval(e2);
    bitset_t RESULT = op == BIN_BITAND ?        (E1 & E2) :
                      (op == BIN_BITOR ?        (E1 | E2) :
                      (op == BIN_MINUS ?        (E1 - E2) :
                                                (E1 ^ E2)));
    bool remember = false;
    CDOUT (*expr << ", E1: " << E1 << ", E2: " << E2 << ", RESULT: " << RESULT);
    if ((value_really_not_known(E1, e1) || value_really_not_known(E2, e2)) &&
        RESULT.valueKnown()) {
        CDOUT("E1/E2: not really known; Result: known");
        if (expr->isE_assign()) {
            CDOUT("Expr: isE_assign");
            E_unary const *uny2 = strip_casts(e2)->ifE_unaryC();
            if (uny2 == NULL) {
                // a '!' may be buried inside a literal
                if (E_intLit const *intLit2 = strip_casts(e2)->ifE_intLitC()) {
                    if (intLit2->original_expr) {
                        VXCOV;
                        uny2 = strip_casts(intLit2->original_expr)->ifE_unaryC();
                    } else {
                        VXCOV;
                    }
                } else {
                    VXCOV;
                }
            } else {
                VXCOV;
            }
            if (uny2) {
                if (uny2->op == UNY_NOT) {
                    // make sure the alternative we're about to suggest is
                    // actually better (and not just differently wrong)
                    bitset_t E2_redo = ~eval(skipCompilerGeneratedBoolification(
                                                 uny2->expr));
                    bitset_t RESULT_redo = redo(op, E1, E2_redo);
                    if (checkRedo(E1, E2_redo, RESULT_redo)) {
                        VXCOV;
                        error = createErrorWithEventAndElaboration(
                            "logical_vs_bitwise",
                            "logical_vs_bitwise",
                            MESSAGE(
                                   E(expr) << " always assigns "
                                   << hexOnIt(RESULT.unsignedValue())
                                   << " to " << E(e1)
                                   << "."),
                            MESSAGE("Did you intend to use '~' rather than "
                                   << "'!'?"));
                    } else {
                        // So far, can't come up with a case that exercises
                        // this.
                        VCOV;
                    }
                } else {
                    VXCOV;
                }
            } else {
                VXCOV;
            }
            if (error == NULL) {
                if (report_unnecessary_op_assign) {
                    VXCOV;
                    char opChar = (op == BIN_BITAND) ?
                        '&' : ((op == BIN_BITOR) ? '|' : ((op ==BIN_MINUS) ? '-': '^'));
                    error = createErrorWithEvent("unnecessary_op_assign",
                        "unnecessary_op_assign",
                        MESSAGE(
                               E(expr) << " always assigns "
                               << hexOnIt(RESULT.unsignedValue())
                               << " to " << E(e1)
                               << ", regardless of its old value, so the '"
                               << opChar << "=' could be replaced with a "
                               << "simple assignment ('=')."));
                } else {
                    VXCOV;
                    remember = true;
                }
            } else {
                VXCOV;
            }
        } else {   //NOT E_assign
            VXCOV;
        }
        if (error == NULL && !remember) {
            if (op == BIN_BITAND &&
                ((isNonLogical0(E1, e1) || isNonLogical0(E2, e2)) || arcnetHack(e1, e2))) {
                if (reportBitAndWithZero() &&
                    (report_bit_and_with_zero_in_macros ||
                     !curTraversal->fn_trav().is_in_macro(expr))) {
                    VXCOV;
                    error = createErrorWithEventAndContext(
                        "bit_and_with_zero",
                        "bit_and_with_zero",
                        MESSAGE(E(expr) << " is always 0."),
                        expr);
                } else {
                    VXCOV;
                    remember = true;
                }
            } else {
                char const   *tag         = "result_independent_of_operands";
                char const   *category    = "result_independent_of_operands";
                event_stream &es          = desc() << E(expr) << " is always ";

                if (RESULT.isSigned()) {
                    VXCOV;
                    es << hexOnIt(RESULT.signedValue());
                } else {
                    VXCOV;
                    es << hexOnIt(RESULT.unsignedValue());
                }
                es << " regardless of the values of its operands.";
                string event_message = consume_event_stream(es);
                string elaboration, elaboration2;

                Binop binop(_, _), &binop1 = binop, &binop2 = binop;
                Binop lshift(BIN_LSHIFT, _, _), &lshift1 = lshift,
                      &lshift2 = lshift;
                Const_int const_int, &const1 = const_int, &const2 = const_int;
                Unop not_pat(UNY_NOT, _);
                Const_int const_not(not_pat);
                // Look for cases where mistaken operator precedence prevented
                // two constants from being combined with a lower-precedence
                // operator.  In these cases 'e1' ('e2') will be a constant
                // ("literal") and 'e2' ('e1') will be an operator with
                // precedence higher than 'op''s, where that operator's
                // operand that is "next to" 'e1' ('e2') will also be a constant
                // ("literal").

                // A case we don't yet deal with is
                //  if (const1 & _ ? const2 | const3)
                // where "(const1 & _)" was intended.
                if (const1.match(e1) && !e2->isE_intLit()) {
                    // look for "!constant & _" that was supposed to
                    // be "~constant & _" or "!(constant & _)"
                    if (const_not.match(e1)) {
                        E_unary const *not1 = not_pat.last_expr();
                        bitset_t E1_redo = ~eval(
                            skipCompilerGeneratedBoolification(not1->expr));
                        bitset_t RESULT_redo = redo(op, E1_redo, E2);
                        if (checkRedo(E1_redo, E2, RESULT_redo)) {
                            VXCOV;
                            tag = "logical_vs_bitwise";
                            category = "logical_vs_bitwise";
                            elaboration = MESSAGE(
                               "Did you intend to use '~' "
                               "rather than '!'?  Another "
                               "possibility is that you intended "
                               "to apply '!' to the entire '&' "
                               "expression, which would require "
                               "additional parentheses.");
                            // XXX: No constant folding in JS, so we can't get here
                            sup_assert(isCLike(curTraversal->getLang()));
                            goto elaboration_done;
                        } else {
                            // So far, unable to come up with a test
                            // case for this.
                            VCOV;
                        }
                    } else {
                        VXCOV;
                    }

                    if (lshift2.match(e2)) {
                        E_binary const *biny2 = lshift2.last_expr();
                        bitset_t E2_redo = eval(biny2->e1) >> eval(biny2->e2);
                        bitset_t RESULT_redo = redo(op, E1, E2_redo);
                        if (checkRedo(E1, E2_redo, RESULT_redo)) {
                            VXCOV;
                            tag = "operator_confusion";
                            category = "operator_confusion";
                            elaboration = MESSAGE(
                                "Did you intend to use right-shift "
                                << "('>>') in " << E(biny2) << "?");
                            goto elaboration_done;
                        } else {
                            // So far, unable to come up with a test
                            // case for this.
                            VCOV;
                        }
                    } else {
                        VXCOV;
                    }

                    if (getTruePrecedence(e2) > getTruePrecedence(expr) &&
                        binop2.match(e2)) {
                        E_binary const *biny2 = binop2.last_expr();
                        if (biny2->e1->isE_intLit() || biny2->e1->isE_floatLit()) {
                            bitset_t E1_redo = redo(op, E1, eval(biny2->e1));
                            bitset_t E2_redo = eval(biny2->e2);
                            bitset_t RESULT_redo = redo(biny2->op, E1_redo,
                                                        E2_redo);
                            if (checkRedo(E1_redo, E2_redo, RESULT_redo)) {
                                VXCOV;
                                tag = "missing_parentheses";
                                category = "missing_parentheses";
                                elaboration = MESSAGE(
                                    "Did you intend the '"
                                    << toStr(op, curLang) << "' to apply to " << E(e1)
                                    << " and " << E(biny2->e1)
                                    << " ?");
                                elaboration2 = EMESSAGE(
                                    "If so, parentheses would be "
                                   "required to force this interpretation.");
                                // sup_assert(isCLike(curTraversal->getLang()));
                                goto elaboration_done;
                            } else {
                                VXCOV;
                            }
                        } else {
                            VXCOV;
                        }
                    } else {
                        VXCOV;
                    }
                } else if (!e1->isE_intLit() && const2.match(e2)) {
                    // look for "_ & !constant" that was supposed to
                    // be "_ & ~constant"
                    if (const_not.match(e2)) {
                        E_unary const *not2 = not_pat.last_expr();
                        bitset_t E2_redo = ~eval(
                            skipCompilerGeneratedBoolification(not2->expr));
                        bitset_t RESULT_redo = redo(op, E1, E2_redo);
                        if (checkRedo(E1, E2_redo, RESULT_redo)) {
                            VXCOV;
                            tag = "logical_vs_bitwise";
                            category = "logical_vs_bitwise";
                            elaboration = MESSAGE(
                               "Did you intend to use '~' rather than '!'?");
                            goto elaboration_done;
                        } else {
                            VXCOV;
                        }
                    } else {
                        VXCOV;
                    }

                    if (lshift1.match(e1)) {
                        E_binary const *biny1 = lshift1.last_expr();
                        bitset_t E1_redo = eval(biny1->e1) >> eval(biny1->e2);
                        bitset_t RESULT_redo = redo(op, E1_redo, E2);
                        if (checkRedo(E1_redo, E2, RESULT_redo)) {
                            VXCOV;
                            tag = "operator_confusion";
                            category = "operator_confusion";
                            elaboration = MESSAGE(
                               "Did you intend to use right-shift "
                               << "('>>') in " << E(biny1) << "?");
                            goto elaboration_done;
                        } else {
                            // So far, unable to come up with a test
                            // case for this.
                            VXCOV;
                        }
                    } else {
                        VXCOV;
                    }

                    if (getTruePrecedence(e1) > getTruePrecedence(expr)) {
                        if (binop1.match(e1)) {
                            // (e1e1 op1 e1e2) op e2 ==> e1e1 op1 (e1e2 op e2)
                            E_binary const *biny1 = binop1.last_expr();
                            bitset_t E1_redo = eval(biny1->e1);
                            bitset_t E2_redo = redo(op, eval(biny1->e2), E2);
                            bitset_t RESULT_redo = redo(biny1->op, E1_redo,
                                                        E2_redo);
                            if (checkRedo(E1_redo, E2_redo, RESULT_redo)) {
                                VXCOV;
                                tag = "missing_parentheses";
                                category = "missing_parentheses";
                                elaboration = MESSAGE(
                                   "Did you intend to apply '"
                                   << toStr(op, curLang) << "' to " << E(biny1->e2)
                                   << " and " << E(e2) << "?");
                                elaboration2 = EMESSAGE(
                                   "If so, parentheses would be "
                                   << "required to force this interpretation.");
                                // This can happen in Java,
                                // see jtest-const-expr-rslt/t9()
                                // though the suggestion is wonky
                                goto elaboration_done;
                            } else {
                                VXCOV;
                            }
                        } else {
                            Unop unop1(_);
                            if (unop1.match(e1)) {
                                E_unary const *uny1 = unop1.last_expr();
                                // (op1 e1e) op e2 ==> op1 (e1e op e2)
                                bitset_t E_redo = redo(op, eval(uny1->expr),
                                                       eval(e2));
                                bitset_t RESULT_redo = ::redo(uny1->op, E_redo);
                                if (checkRedo(E_redo, RESULT_redo)) {
                                    VXCOV;
                                    tag = "missing_parentheses";
                                    category = "missing_parentheses";
                                    elaboration = MESSAGE(
                                       "Did you intend to apply '"
                                       << toStr(op, curLang) << "' to "
                                       << E(uny1->expr) << " and " << E(e2)
                                       << "?");
                                    elaboration2 = EMESSAGE(
                                       "If so, parentheses would "
                                       << "be required to force this "
                                       << "interpretation.");
                                    // sup_assert(isCLike(curTraversal->getLang()));
                                    goto elaboration_done;
                                } else {
                                    VCOV;
                                }
                            } else {
                                VXCOV;
                            }
                        }
                    } else {
                        VXCOV;
                    }
                } else {
                    VXCOV;
                }

                if (op == BIN_BITOR && e1->isE_intLit() != e2->isE_intLit() &&
                    // is suggested change better than original?
                    !(eval(e1) & eval(e2)).valueKnown()) {
                    VXCOV;
                    tag = "operator_confusion";
                    category = "operator_confusion";
                    elaboration = MESSAGE(
                       "Did you intend to use '&' rather than '|'?");
                    goto elaboration_done;
                } else {
                    VXCOV;
                }

                E_binary const *left_shift_child;
                if (op == BIN_BITAND) {
                    if ((left_shift_child = e1->ifE_binaryC()) != NULL &&
                        left_shift_child->op == BIN_LSHIFT) {
                        bitset_t E1_redo = eval(left_shift_child->e1) >>
                                           eval(left_shift_child->e2);
                        bitset_t RESULT_redo = redo(op, E1_redo, E2);
                        if (checkRedo(E1_redo, E2, RESULT_redo)) {
                            VXCOV;
                            tag = "operator_confusion";
                            category = "operator_confusion";
                            elaboration = MESSAGE(
                               "Did you intend to use right-shift "
                               << "('>>') in " << E(left_shift_child) << "?");
                            goto elaboration_done;
                        } else {
                            VXCOV;
                        }
                    } else {
                        VXCOV;
                    }
                    if ((left_shift_child = e2->ifE_binaryC()) != NULL &&
                        left_shift_child->op == BIN_LSHIFT) {
                        bitset_t E2_redo = eval(left_shift_child->e1) >>
                                           eval(left_shift_child->e2);
                        bitset_t RESULT_redo = redo(op, E1, E2_redo);
                        if (checkRedo(E1, E2_redo, RESULT_redo)) {
                            VXCOV;
                            tag = "operator_confusion";
                            category = "operator_confusion";
                            elaboration = MESSAGE(
                               "Did you intend to use right-shift "
                               << "('>>') in " << E(left_shift_child) << "?");
                            goto elaboration_done;
                        } else {
                            VXCOV;
                        }
                    } else {
                        VXCOV;
                    }
                } else {
                    VXCOV;
                }
elaboration_done:
                error = createErrorWithEventAndContextAndElaborations(
                    tag, category, event_message, expr, elaboration, elaboration2);
            }
        } else {
            VXCOV;
        }
    } else {
        VXCOV;
    }

    if (error == NULL && expr->isE_assign()) {
        bitype extra = (E2.knownOne & E2.all()) & ~E1.all();
        if (extra != 0 && !is_prefix_bitvector(extra, (E2.all() & ~E1.all()))) {
            VXCOV;
            // e.g., "shortVar &= 0x10000" but not "shortVar &= ~0xffff"
            error = createErrorWithEvent("extra_high_bits",
                "extra_high_bits",
                MESSAGE(
                      "In " << E(expr) << ", wider " << E(e2)
                       << " has high-order bits (" << hexOnIt(extra)
                       << ") that don't affect the narrower left-hand side."));
        } else {
            VXCOV;
        }
    } else {
        VXCOV;
    }

    if (error == NULL && remember) {
        VXCOV;
        rememberReportingError(); //option related, once set, can report additional errors;
    }

    return error;
}


// is 'e' an intLit that's either a straight literal, a cast of a trivial
// literal, or a unary op applied to a trivial literal?
static bool is_trivial_literal(Expression const *e)
{
    if (E_unary const *uny = e->ifE_unaryC()) {
        // -(1), ~0
        VXCOV;
        return is_trivial_literal(uny->expr);
    }

    if (E_cast const *cast = e->ifE_castC()) {
        VXCOV;
        return is_trivial_literal(cast->expr);
    }

    if (E_intLit const *intLit = e->ifE_intLitC()) {
        if (intLit->original_expr == NULL) {
            VXCOV;
            return true;
        }

        VXCOV;
        return is_trivial_literal(intLit->original_expr);
    } else {
        VXCOV;
    }

    return false;
}


// Is 'e1' 'op' 'e2' either a comparison of an unsigned expression explicitly
// against literal zero or implicitly against zero (</> literal -1), or against
// the largest possible value for its type?
static bool is_size_based_bounds_check(Expression const *e1,
                                       BinaryOp op,
                                       Expression const *e2)
{
    int lowerBoundForPositiveLogic, lowerBoundForNegativeLogic;
    // TODO: these names are wrong; unlike the lower bounds, these don't
    // indicate whether to check an upper bound that would be checked with
    // negated logic; instead they indicate which "polarity" of *lower* bound
    // check the upper bounds check needs to be done along with in order for it
    // to get the 'e' and the 'intLit' right
    bool checkUpperBoundPos, checkUpperBoundNeg;
    switch (op) {
    case BIN_LESS:
        // -1 < us
        lowerBoundForPositiveLogic = -1;
        // !(us < 0)
        lowerBoundForNegativeLogic = 0;
        // !(0xffff < us)
        checkUpperBoundPos = true;
        checkUpperBoundNeg = false;
        VXCOV;
        break;

    case BIN_LESSEQ:
        // 0 <= us
        lowerBoundForPositiveLogic = 0;
        // !(us <= -1)
        lowerBoundForNegativeLogic = -1;
        // us <= 0xffff
        checkUpperBoundPos = false;
        checkUpperBoundNeg = true;
        VXCOV;
        break;

    case BIN_GREATER:
        VXCOV;
        // reverse operands and reuse < logic
        return is_size_based_bounds_check(e2, BIN_LESS, e1);
        break;

    case BIN_GREATEREQ:
        VXCOV;
        // reverse operands and reuse <= logic
        return is_size_based_bounds_check(e2, BIN_LESSEQ, e1);
        break;

    default:
        VXCOV;
        return false;
    }

    // Look for the positive logic versions:
    if (is_trivial_literal(e1)) {
        E_intLit const *intLit =
            static_cast<E_intLit const *>(e1);
        type_t const *t = strip_casts(e2)->type;
        if (intLit->i == lowerBoundForPositiveLogic) {
            if (scalar_type_t const *st = t->as_scalar_p()) {
                if (st->is_unsigned()) {
                    VXCOV;
                    return true;
                } else {
                    VCOV;
                }
            } else if (t->as_enum_p()) {
                // conservatively assume that enum types are unsigned, at
                // least to the extent that we won't report comparing an
                // enum against 0
                VXCOV;
                return true;
            } else {
                // addr literal
                VCOV;
            }
        } else if (checkUpperBoundPos) {
            if (scalar_type_t const *st = t->as_scalar_p()) {
                if (st->is_unsigned()) {
                    unsigned long long upperBoundForPositiveLogic =
                        (1ULL << (st->get_size() * 8)) - 1;
                    if (intLit->i == upperBoundForPositiveLogic) {
                        VXCOV;
                        return true;
                    } else {
                        VCOV;
                    }
                } else {
                    VCOV;
                }
            } else {
                VCOV;
            }
        } else {
            VCOV;
        }
    } else {
        VXCOV;
    }

    // Look for the negative logic versions:
    if (is_trivial_literal(e2)) {
        E_intLit const *intLit =
            static_cast<E_intLit const *>(e2);
        type_t const *t = strip_casts(e1)->type;
        if (intLit->i == lowerBoundForNegativeLogic) {
            if (scalar_type_t const *st = t->as_scalar_p()) {
                if (st->is_unsigned()) {
                    VXCOV;
                    return true;
                } else {
                    VXCOV;
                }
            } else if (t->as_enum_p()) {
                // conservatively assume that enum types are unsigned, at
                // least to the extent that we won't report comparing an
                // enum against 0
                VXCOV;
                return true;
            } else {
                // addr literal
                VCOV;
            }
        } else if (checkUpperBoundNeg) {
            if (scalar_type_t const *st = t->as_scalar_p()) {
                if (st->is_unsigned()) {
                    unsigned long long upperBoundForNegativeLogic =
                        (1ULL << (st->get_size() * 8)) - 1;
                    if (intLit->i == upperBoundForNegativeLogic) {
                        VXCOV;
                        return true;
                    } else {
                        VXCOV;
                    }
                } else {
                    VCOV;
                }
            } else {
                VCOV;
            }
        } else {
            VXCOV;
        }
    } else {
        VXCOV;
    }

    return false;
}


static bool is_size_based_bounds_check_and_not_loop_condition(E_binary const *biny)
{
    if (is_size_based_bounds_check(biny->e1, biny->op, biny->e2)) {
        // If 'biny' is a loop bound we still want to complain about it, since
        // the consequences of a genuine error of this sort on loop bounds are
        // likely to be much greater.  If it is being returned that return value
        // might be used as a loop condition, so include them, too.

        // Allow for a complex conditional:
        Expression const *e = biny;
        Expression const *parentExpr;
        while ((parentExpr = e->ifExpressionParent()) != NULL &&
               (parentExpr->isBinary(BIN_AND) || parentExpr->isBinary(BIN_OR) ||
                parentExpr->isUnary(UNY_NOT))) {
            VXCOV;
            e = parentExpr;
        }

        // Look for a looping or return statment:
        if (Statement const *stmtParent = e->parent->ifStatementC()) {
            ASTSWITCH(stmtParent) {
            ASTCASE1(S_while) {
                VXCOV;
                return false;
            }
            ASTCASE1(S_doWhile) {
                VXCOV;
                return false;
            }
            ASTCASE1(S_for) {
                // don't fret over "for (a<0xff; ...)" or "for (...; a<0xff)"
                VXCOV;
                return false;
            }
            ASTCASE1(S_return) {
                VXCOV;
                return false;
            }
            ASTENDCASED;
                VXCOV;
            }
        } else {
            VCOV;
        }

        return true;
    } else {
        VXCOV;
    }

    return false;
}


static bool is_relop(Expression const *e)
{
    if (E_binary const *biny = e->ifE_binaryC()) {
        switch (biny->op) {
        case BIN_EQUAL:
        case BIN_NOTEQUAL:
        case BIN_DYNAMIC_EQUAL:
        case BIN_DYNAMIC_NOTEQUAL:
        case BIN_LESS:
        case BIN_LESSEQ:
        case BIN_GREATER:
        case BIN_GREATEREQ:
            VXCOV;
            return true;

        default: VCOV;
        }
    } else {
        VXCOV;
    }

    return false;
}



void ConstantExpressionResult::handle_tree(
    state_t *state,
    const ASTNode *t,
    abstract_interp_t &cur_traversal)
{
#if CER_USE_STORE
    // this was lifted pretty much literally from bitfpp; if we keep it it
    // needs to be merged with the above code (most likely) in order not to
    // be redundant (and then there's the whole factoring thing...)
    {
    bit_store_t *store = static_cast<bit_store_t*>(get_state());

    BEGIN_PATTERNS;
    static Pointer ptr;
    static FunLocal loc;
    static Expr any;

    // Don't track pointers, handled by notnull_fpp
    static DECL_PAT(e, And(Not(ptr), Or(loc, any.field())));
    static Expr rhs;
    static Assign assign(e, rhs, Assign::AF_MATCH_ANY_OPERATOR);
    END_PATTERNS;

    if((e = rhs).match(t) || assign.match(t)) {
        bitset_t b = eval(rhs);
        bit_store_t::iterator p = store->find(e);
        if(p != store->end()) {
            cout << "updating " << p->second << " in store for " << *e << endl;
            bitset_widen()(NULL, e, b, p->second);
        } else {
            cout << "storing " << b << " for " << e << endl;
        }
        store->insert(e, b, true);
        if(/*debug::bit_fpp*/false) {
            store->write_as_text(cout << endl << "C_E_R: =: " << endl
                                 << t << endl);
        }
    } else if(OrMatch(t, ++e, e++, --e, e--, &e )) {
        // we could track effect operations, but they tend to cause more cache
        // misses than they're worth
        store->set_all_insert(e, bitset_t::bottom);
        //cout << "clobbering " << e << endl;
        if(/*debug::bit_fpp*/false) {
            store->write_as_text(cout << endl << "C_E_R: setting to bottom: " << endl << t << endl);
        }
    }
    }
#endif /* CER_USE_STORE */
}

static bool same_no_side_effect(Expression const *e1, Expression const *e2);

static bool same_if_literals(Expression const *e1, Expression const *e2)
{
    E_intLit const *lit1 = e1->ifE_intLitC();
    E_intLit const *lit2 = e2->ifE_intLitC();
    if (lit1 != NULL && lit2 != NULL) {
        if (lit1->original_expr != NULL) {
            if (lit2->original_expr != NULL) {
                E_cast const *c1 = lit1->original_expr->ifE_castC();
                E_cast const *c2 = lit2->original_expr->ifE_castC();
                if (c1 != NULL && c2 != NULL) {
                    if (c1->expr->type != c2->expr->type) {
                        VXCOV;
                        return false;
                    } else {
                        VXCOV;
                    }
                } else {
                    VXCOV;
                }
                return same_no_side_effect(lit1->original_expr,
                                           lit2->original_expr);
            } else {
                VXCOV;
                return false;
            }
        } else if (lit2->original_expr) {
            VXCOV;
            return false;
        } else {
            VXCOV;
            return true;
        }
    } else {
        VXCOV;
        return true;
    }
    // unreachable, but shuts up stupid compiler warning
    return true;
}

static bool improved_has_side_effects(Expression const *e)
{
    if (!has_side_effects(e)) {
        return false;
    }
    // Some function calls are known to have no side effects.  This is by
    // no means an exhaustive list.
    BEGIN_PATTERNS;
    static Expr receiver;
    static CallSite csharp_GetType_base("GetType");
    static DECL_PAT(csharp_GetType_call, csharp_GetType_base(receiver));
    END_PATTERNS;
    if (csharp_GetType_call.match(e) && !improved_has_side_effects(receiver)) {
        return false;
    }
    return true;
}

static bool same_no_side_effect(Expression const *e1, Expression const *e2)
{
    VXCOV;
    e1 = find_outermost_cast(e1, true /* explicit_only */);
    e2 = find_outermost_cast(e2, true /* explicit_only */);
    Expression const *ev1 = Evals_to(e1, EO_KEEP_CASTS);
    Expression const *ev2 = Evals_to(e2, EO_KEEP_CASTS);
    return expr_full_compare(ev1, ev2, true /*deep_compare_literals*/) == 0 &&
        same_if_literals(ev1, ev2) &&
        // Note: we need to check both sides for side effects because even
        // when they're "the same" one may have had side effects Evals_to'd
        // away.
        !improved_has_side_effects(e1) && !improved_has_side_effects(e2);
}

static bool is_in_any_macro(Expression const *e,
                            abstract_interp_t *cur_traversal)
{
    VXCOV;
    const char *xxx_name;
    EmitSourceLoc xxx_loc;
    LETREF(tv, cur_traversal->fn_trav());
    return tv.is_in_macro(e, MACRO_FUNCTION, xxx_name, xxx_loc) ||
        tv.is_in_macro(e, MACRO_CONSTANT, xxx_name, xxx_loc) ||
        tv.is_in_macro(e, MACRO_PLAIN, xxx_name, xxx_loc);
}

static bool isJavaScriptGlobalVariable(Expression const *e) {
    if(e->isE_mapAcc() &&
        e->ifE_mapAccC()->map->isE_variable() &&
        e->ifE_mapAccC()->map->asE_variable()->var->isglobal_variable_t()) {
        return true;
    }
    return false;
}

static bool any_sobs_exclusions(Expression const *expr, Expression const *e1,
                                Expression const *e2,
                                abstract_interp_t *curTraversal)
{
    Float float_val;
    if (float_val.match(e1)) {
        VXCOV;
        return true;
    }

    if (is_in_any_macro(expr, curTraversal)) {
        VXCOV;
        return true;
    }

    // same_expr_memory isn't very reliable with sizeofs,
    // and we want to consider sizeofs to be different unless they're
    // textually identical -- which is difficult to determine -- so
    // we just punt if there are any sizeofs involved at all.
    SizeofPattern szof;
    Const_int sizeof_lit(szof);
    if (Contains(sizeof_lit).match(e1) || Contains(sizeof_lit).match(e2)) {
        VCOV;
        return true;
    }

    // Don't report any SOBS involving constants within instantiations
    // of templates with non-type template parameters for fear of FPs
    // involving the template arguments (which we currently can't tell from
    // plain integer literals).
    Const_int any_int_lit;
    if ((Contains(any_int_lit).match(e1) || Contains(any_int_lit).match(e2)) &&
        check_for_integral_template_argument(curTraversal->current_fn_name())) {
        VXCOV;
        return true;
    }

    // Statement expressions can hide all sorts of magic, so steer clear of them.
    StmtExpr stmt_expr;
    if (Contains(stmt_expr).match(e1)) {
        VCOV;
        return true;
    }

    // In Javascript, it's very common to check if foo != foo. This is true
    // for NaN and is an alternative to using (typeof foo === 'number' && isNaN(foo))
    if (isJavaScript(curTraversal->getLang())) {
        if(expr->isE_binary() && (expr->ifE_binaryC()->op == BIN_LESSEQ ||
            expr->ifE_binaryC()->op == BIN_GREATEREQ ||
            expr->ifE_binaryC()->op == BIN_DYNAMIC_EQUAL ||
            expr->ifE_binaryC()->op == BIN_DYNAMIC_NOTEQUAL ||
            expr->ifE_binaryC()->op == BIN_NOTEQUAL ||
            expr->ifE_binaryC()->op == BIN_EQUAL)) {
                return true;
        }
        if(expr->isE_cond() &&
            expr->ifE_condC()->condKind == CEK_JAVASCRIPT_OR &&
            ((e1->isE_binary() && (
            e1->ifE_binaryC()->op == BIN_DYNAMIC_EQUAL ||
            e1->ifE_binaryC()->op == BIN_EQUAL ||
            e1->ifE_binaryC()->op == BIN_DYNAMIC_NOTEQUAL ||
            e1->ifE_binaryC()->op == BIN_NOTEQUAL)) ||
            (e2->isE_binary() && (
                e2->ifE_binaryC()->op == BIN_DYNAMIC_EQUAL ||
                e2->ifE_binaryC()->op == BIN_EQUAL ||
                e2->ifE_binaryC()->op == BIN_DYNAMIC_NOTEQUAL ||
                e2->ifE_binaryC()->op == BIN_NOTEQUAL)))) {
            return true;
        }
        // Common FP pattern in JS to use foo[a] || foo.a to
        // get around browser incompatibilities
        if(expr->isE_cond() && expr->ifE_condC()->condKind == CEK_JAVASCRIPT_OR &&
            e1->isE_mapAcc() && !isJavaScriptGlobalVariable(e1)
            && e2->isE_mapAcc() && !isJavaScriptGlobalVariable(e2)
            && ((e1->asE_mapAccC()->notation == MAP_ACCESS_NOTATION_DOT) ^
                (e2->asE_mapAccC()->notation == MAP_ACCESS_NOTATION_DOT))) {
            return true;
        }
    }

    VXCOV;
    return false;
}

// If "t" is a scalar type, return true and set "is_unsigned" to tell
// whether it's unsigned.
static bool getScalarSign(
    const type_t *t,
    bool &is_unsigned,
    enum_loader_t &enumLoader)
{
    TYPESWITCH(t) {
        TYPECASE(scalar_type_t, s) {
            is_unsigned = s->is_unsigned();
            return true;
        }
        TYPECASE(enum_type_t, e) {
            defined_enum_type_t d = enumLoader.load(e);
            if(!d.is_present()) {
                return false;
            }
            is_unsigned = d->get_underlying_type()->is_unsigned();
            return true;
        }
        TYPEENDCASED;
    }
    return false;
}

static bool isUnsignedZero(const Expression *e, enum_loader_t &enumLoader)
{
    const E_intLit *iLit = e->ifE_intLitC();
    if(!iLit || iLit->i) {
        return false;
    }
    bool is_unsigned = false;
    return getScalarSign(iLit->type, is_unsigned, enumLoader) && is_unsigned;
}

bool ConstantExpressionResult::isUnsignedCompareZero(const E_binary *biny)
{
    if(!isInequality(biny->op)) {
        VXCOV;
        return false;
    }
    const Expression *e1 = biny->e1;
    const Expression *e2 = biny->e2;
    BinaryOp op = biny->op;
    bool swapped = false;
    // Normalize so that the unsigned 0 is "e2"
    enum_loader_t &enumLoader = curTraversal->get_type_loader();
    if(isUnsignedZero(e1, enumLoader)) {
        VXCOV;
        std::swap(e1, e2);
        op = swapRelOpOperands(op);
        swapped = true;
    } else if(!isUnsignedZero(e2, enumLoader)) {
        VXCOV;
        return false;
    }
    if(op != BIN_LESS && op != BIN_GREATEREQ) {
        VXCOV;
        return false;
    }
    if (swapped) {
      VXCOV;
      // GCC UPGRADE 4.6.3
      // Unused variable warning = error
    }
    VXCOV;
    return true;
}

static bool isConstantOtherThan0Or1(Expression const *e)
{
    if (E_binary const *bny = e->ifE_binaryC()) {
        // Strip away compiler-generated bool-ification.
        if (bny->op == BIN_NOTEQUAL && bny->isImplicit == PI_IMPLICIT) {
            VXCOV;
            e = bny->e1;
        } else {
            VXCOV;
        }
    }
    if (E_intLit const *il = e->ifE_intLitC()) {
        VXCOV;
        return (il->i != 0 && il->i != 1) ||
               (il->original_expr != NULL &&
                isConstantOtherThan0Or1(il->original_expr));
    }
    if (E_floatLit const *fl = e->ifE_floatLitC()) {
        return (fl->original_expr != NULL &&
                isConstantOtherThan0Or1(fl->original_expr));
    }
    VXCOV;
    return false;
}

static bool isExplicitNonBitwiseBinary(Expression const *e)
{
    E_binary const *bny = e->ifE_binaryC();
    if (bny == NULL) {
        VXCOV;
        return false;
    }
    // Technically, &, |, ^ are also bitwise, but I don't want to include
    // them until I see an actual example of them and see whether the
    // remediation would make sense in those cases.
    if (bny->op == BIN_LSHIFT || bny->op == BIN_RSHIFT) {
        VXCOV;
        return false;
    }
    VXCOV;
    return bny->isImplicit == PI_EXPLICIT;
}

err_t *ConstantExpressionResult::checkBitwiseVsLogicalAndOr(
    Expression const *expr,
    Expression const *e1,
    Expression const *e2,
    bool is_andand)
{
    bool e1_const = isConstantOtherThan0Or1(e1);
    bool e2_const = isConstantOtherThan0Or1(e2);
    // At least one side needs to be a constant, and both sides being constant
    // is often intentional, e.g. "if (COND_FLAGS || DBG_FLAGS) ...".
    if ((!e1_const && !e2_const)
        || ((e1->isE_intLit() || e1->isE_floatLit())
            && (e2->isE_intLit() || e2->isE_floatLit()))) {
        VXCOV;
        return NULL;
    }

    // Sometimes people write
    //  if (FOO && bar)
    // as shorthand for
    //  if (FOO != 0 && bar)
    // so don't report if the first operand of && is a constant and macro
    // is involved.
    if (is_andand && e1_const && !e2_const &&
        is_in_any_macro(e1, curTraversal)) {
        VXCOV;
        return NULL;
    }

    if(isJavaScript(curTraversal->getLang())) {
        if (e1_const && (!(e2->isE_binary() || e2->isE_unary()))) {
            return NULL;
        }

        // People use constants in JS all the time as defaults. So skip those.
        if(e2_const) {
            return NULL;
        }
    }

    VXCOV;
    err_t *error = create_nopath_error(*curTraversal);
    error->add_event("logical_vs_bitwise",
                     desc() << "The expression " << E(expr)
                     << " is suspicious because it performs "
                     << "a Boolean operation on a constant "
                     << "other than 0 or 1.",
                     curTree);
    error->set_main_event();
    // People write weird (wrong) code like:
    //   if (a == FOO || BAR)
    // where the problem isn't &&/|| vs. &/| confusion.  Maybe we shouldn't
    // call this "logical_vs_bitwise" at all, but let's at least not double
    // down on that with misguided remediation advice.
    //
    // TODO: don't suggest || in contexts like "if (e || 3)".
    if (!((e1_const && isExplicitNonBitwiseBinary(e2)) ||
          (isExplicitNonBitwiseBinary(e1) && e2_const))) {
        VXCOV;
        error->add_remediation_event("remediation",
                                     desc() << "Did you intend to "
                                         << "use "
                                         << (is_andand ? "'&'" :
                                                         "'|'")
                                         << " rather than "
                                         << (is_andand ? "'&&'" :
                                                         "'||'")
                                         << "?",
                                     curTree);
    } else if (E_binary const *bny1 = e1->ifE_binaryC()) {
        // When we can recognize the likely "a != FOO && BAR" and
        // "a == FOO || BAR" cases, provide appropriate remediation.
        // Similarly, "... || a == FOO || BAR", where the comparison is
        // buried down in the tree on the LHS.
        if (((is_andand && bny1->op == BIN_AND) ||
             (!is_andand && bny1->op == BIN_OR)) &&
            bny1->e2->isE_binary()) {
            VXCOV;
            bny1 = static_cast<E_binary const *>(bny1->e2);
        } else {
            VXCOV;
        }
        Expression const *comparand = bny1->e1;
        // TODO: Create the ASTs for the intended expressions and
        // print it as part of the remediations.
        if (is_andand && (bny1->op == BIN_NOTEQUAL || bny1->op == BIN_DYNAMIC_NOTEQUAL)) {
            VXCOV;
            error->add_remediation_event("remediation",
                                         desc() << "Did you intend to "
                                             << "also compare "
                                             << *comparand << " to "
                                             << *e2 << "?",
                                         curTree);
        } else if (!is_andand && (bny1->op == BIN_EQUAL || bny1->op == BIN_DYNAMIC_EQUAL)) {
            VXCOV;
            error->add_remediation_event("remediation",
                                         desc() << "Did you intend to "
                                             << "also compare "
                                             << *comparand << " to "
                                             << *e2 << "?",
                                         curTree);
        } else {
            VXCOV;
        }
    } else {
        // Is this dead?
        VCOV;
    }
    error->set_subcategory("logical_vs_bitwise");
    return error;
}

static bool parent_is_same(E_binary const *bny)
{
    VXCOV;
    assert(bny->parent);
    Expression const *parent_expr = bny->parent->ifExpressionC();
    return parent_expr != NULL && parent_expr->isBinary(bny->op);
}

class ComparandCollector {
public:
    ComparandCollector(E_binary const *bny,
                       bool allow_comparison_to_non_constant) :
        mySideUsed(NeitherSide),
        myAllowComparisonToNonConstant(allow_comparison_to_non_constant),
        myParentOp(bny->op)
    {
        VXCOV;
        recursiveCollectComparands2(bny);
    }

    err_t *reportError(checker_t *checker, abstract_interp_t *cur_traversal,
                       Expression const *expr, bool is_andand)
    {
        // Process the comparands pair-wise, looking for any
        // two that are the same.
        // 'i' is actually "i+1"
        for (int i = 1; i < myComparands.size(); ++i) {
            for (int j = i; j < myComparands.size(); ++j) {
                Expression const *ei = myComparands[i - 1];
                Expression const *ej = myComparands[j];
                if (!same_no_side_effect(ei, ej)) {
                    continue;
                }

                // We don't want to report same-on-both-sides cases here,
                // both because that's often redundant with the SOBS reporting
                // done elsewhere by this checker and because in those cases
                // the impossible/tautology claim isn't accurate.
                Expression const *bini = ei;
                Expression const *binj = ej;
                do {
                    bini = static_cast<Expression const *>(bini->parent);
                } while (!bini->isE_binary());
                do {
                    binj = static_cast<Expression const *>(binj->parent);
                } while (!binj->isE_binary());
                if (same_no_side_effect(bini, binj)) {
                    continue;
                }

                // Avoid FPs on things like
                //  x == 1 && (x = ...) && x == 2
                //  x == 1 && ++x && x == 2
                //  x == 1 && f(&x) && x == 2
                //  this->err==0 && foo() && this->error==1
                // while allowing strcmp(), equals(), etc.
                Assign assign(Same(ei), _);
                Effect effect(Same(ei));
                NonConstAddr addr(Same(ei));
                CallSite any_call; // conservative
                CallSite strcmp_call("strcmp");
                NamedSymbolByRegex equal_sym("[Ee]qual");
                PossiblyOverloadedBinop operator_equal(BIN_EQUAL, _, _);
                PossiblyOverloadedBinop operator_not_equal(BIN_NOTEQUAL, _, _);
                CallSite equal_call(equal_sym);
                CallSite csharp_GetType_call("GetType");
                Constructor ctor;
                Destructor dtor;
                DECL_PAT(ctor_dtor, Or(ctor, dtor));
                DECL_PAT(ok_calls, Or(strcmp_call, operator_equal,
                                      equal_call, operator_not_equal,
                                      csharp_GetType_call, ctor_dtor));
                DECL_PAT(bad_calls, And(any_call, Not(ok_calls)));
                DECL_PAT(unwanted, Or(assign, effect, addr, bad_calls));
                if (ExprContains(unwanted).match(expr)) {
                    CDOUT_FULL(checker,
                               *expr << " excluded because it contains " <<
                                   unwanted);
                    VXCOV;
                    continue;
                }

                err_t *error = checker->create_nopath_error(*cur_traversal);
                if (is_andand) {
                    VXCOV;
                    error->add_event("impossible_and",
                                     desc()
                                         << "The \"and\" condition "
                                         << *expr
                                         << " can never be true because "
                                         << *ei
                                         << " cannot be equal to two different "
                                         << "values at the same time.",
                                     expr);
                } else {
                    VXCOV;
                    error->add_event("always_true_or",
                                     desc()
                                         << "The \"or\" condition "
                                         << *expr
                                         << " will always be true because "
                                         << *ei
                                         << " cannot be equal to two different "
                                         << "values at the same time, so it "
                                         << "must be not equal to at least one "
                                         << "of them.",
                                      expr);
                }
                error->set_main_event();
                return error;
            } // for j
        } // for i

        return NULL;
    }
private:
    // Which side of comparisons, if any (yet), are we using?  We can use
    // either one, i.e. "a == 1 && a == 2" or "1 == a && 2 == a", but we need
    // to be consistent once we've picked a side to use.  Actually, I suppose
    // that's not quite right: if there's a clear asymmetry between the
    // suitability of one side as a comparand and the other as compared-to then
    // we could just use the choice that "works" for any given comparison, only
    // worrying about consistency when there's no clear choice.
    enum {NeitherSide, LeftSide, RightSide} mySideUsed;

    // Collect comparands either from a comparison or from under a nested
    // conjunction/disjunction of the same kind ('myParentOp') as the parent.
    // We're only interested in comparisons that are wrong to repeat with the
    // given {con,dis}junction.
    void recursiveCollectComparands1(Expression const *e)
    {
        BEGIN_PATTERNS;
        static Expr strcmpee;
        static Expr second_arg;
        static CallSite strcmp_base("strcmp");
        static DECL_PAT(strcmp_call, strcmp_base(strcmpee, second_arg));
        static Const_int zero(0);
        // Exclude sizeof expressions from "constants" because they may not
        // be constant across different builds.
        static SizeofPattern sizeof_pat;
        static Const_int const_int(Preclude(sizeof_pat));
        static Const_float const_float;
        static Const_string const_string;
        static JavaEnumerator java_enumerator;
        static ClassLit class_lit;
        static Constructor ctor;
        static DECL_PAT(_const_anything,
                        Or(const_int, const_float, const_string,
                           java_enumerator, class_lit));
        // Treat anything built from a single constant as also being a
        // "constant".
        static TempInit temp_from_constant(_const_anything);
        static DECL_PAT(const_anything, Or(_const_anything,
                                           &temp_from_constant));
        static NamedSymbolByRegex equal_sym("[Ee]qual");
        // These are C# overloaded operator names that contain the string
        // "equal" but are *not* equality.
        // TODO: use getOverloadedCSMethodNameFromBinaryOp() to get names
        static NamedSymbolByRegex csharp_exclusions_sym(
            "op_Inequality|op_LessThanOrEqual|op_GreaterThanOrEqual");
        static CallSite equal_base(And(equal_sym, Not(csharp_exclusions_sym)));
        static Expr equalee;
        static PossiblyOverloadedBinop operator_equal(BIN_EQUAL,
                                                      equalee, second_arg);
        static DECL_PAT(equal_call, Or(equal_base(equalee, second_arg),
                                       operator_equal));
        static PossiblyOverloadedBinop operator_not_equal(BIN_NOTEQUAL,
                                                          equalee, second_arg);
        static NamedSymbol csharp_not_equal_sym("op_Inequality");
        static CallSite csharp_not_equal_base(csharp_not_equal_sym);
        static DECL_PAT(csharp_not_equal_call,
                        csharp_not_equal_base(equalee, second_arg));
        static DECL_PAT(not_equal_call, Or(operator_not_equal,
                                           csharp_not_equal_call));
        static Lval lval;
        static DECL_PAT(comparand_pat, And(lval, Not(const_anything)));
        END_PATTERNS;
        ExpressionPattern &compared_to_pat = myAllowComparisonToNonConstant ?
                                                 lval : const_anything;

        if (E_binary const *bny = e->ifE_binaryC()) {
            if (bny->op == ((myParentOp == BIN_AND) ? BIN_EQUAL : BIN_NOTEQUAL)) {
                Expression const *comparand = NULL;
                Expression const *compared_to = NULL;
                switch (mySideUsed) {
                case NeitherSide:
                    // TODO: even the strcmp's args could be "reversed"
                    if (strcmp_call.match(bny->e1) &&
                        compared_to_pat.match(second_arg)) {
                        if (zero.match(bny->e2)) {
                            VXCOV;
                            comparand = bny->e1;
                            compared_to = bny->e2;
                        } else {
                            VXCOV;
                        }
                    } else if (comparand_pat.match(bny->e1) &&
                               compared_to_pat.match(bny->e2)) {
                        VXCOV;
                        comparand = bny->e1;
                        compared_to = bny->e2;
                    } else {
                        VXCOV;
                    }
                    if (comparand != NULL) {
                        VXCOV;
                        mySideUsed = LeftSide;
                    } else {
                        if (strcmp_call.match(bny->e2) &&
                            compared_to_pat.match(second_arg)) {
                            if (zero.match(bny->e1)) {
                                VXCOV;
                                comparand = strcmpee;
                                compared_to = second_arg;
                            } else {
                                VXCOV;
                            }
                        } else if (comparand_pat.match(bny->e2) &&
                                   compared_to_pat.match(bny->e1)) {
                            VXCOV;
                            comparand = bny->e2;
                            compared_to = bny->e1;
                        } else {
                            VXCOV;
                        }
                        if (comparand != NULL) {
                            VXCOV;
                            mySideUsed = RightSide;
                        } else {
                            VXCOV;
                        }
                    }
                    break;

                case LeftSide:
                    VXCOV;
                    comparand = bny->e1;
                    compared_to = bny->e2;
                    break;

                case RightSide:
                    VXCOV;
                    comparand = bny->e2;
                    compared_to = bny->e1;
                    break;
                }
                if (comparand != NULL) {
                    if (strcmp_call.match(comparand) &&
                        compared_to_pat.match(second_arg)) {
                        if (zero.match(compared_to)) {
                            VXCOV;
                            myComparands.push_back(strcmpee);
                        } else {
                            // Unreachable?
                            VCOV;
                        }
                    } else if (comparand_pat.match(comparand) &&
                               compared_to_pat.match(compared_to)) {
                        VXCOV;
                        myComparands.push_back(comparand);
                    } else {
                        VXCOV;
                    }
                } else {
                    VXCOV;
                }
            } else if (bny->op == ((myParentOp == BIN_AND) ?
                                        BIN_NOTEQUAL : BIN_EQUAL)) {
                if (equal_call.match(bny->e1) &&
                    compared_to_pat.match(second_arg) && zero.match(bny->e2)) {
                    VXCOV;
                    myComparands.push_back(equalee);
                } else {
                    VXCOV;
                }
            } else if (bny->op == myParentOp) {
                VXCOV;
                recursiveCollectComparands2(bny);
            } else {
                VXCOV;
            }
        } else if (myParentOp == BIN_OR) {
            if (strcmp_call.match(e) && compared_to_pat.match(second_arg)) {
                // maybe not possible?
                VCOV;
                myComparands.push_back(strcmpee);
            } else if (not_equal_call.match(e) &&
                       compared_to_pat.match(second_arg)) {
                VXCOV;
                myComparands.push_back(equalee);
            } else if (e->isUnary(UNY_NOT) &&
                       equal_call.match(skipCompilerGeneratedBoolification(
                           static_cast<E_unary const*>(e)->expr)) &&
                       compared_to_pat.match(second_arg)) {
                VXCOV;
                myComparands.push_back(equalee);
            } else {
                VXCOV;
            }
        } else {
            assert(myParentOp == BIN_AND);
            if (equal_call.match(e) && compared_to_pat.match(second_arg)) {
                VXCOV;
                myComparands.push_back(equalee);
            } else if (e->isUnary(UNY_NOT) &&
                       // Let's assume no one's going to write
                       //  !(foo != "bar") /* ==> !foo.op_Inequality("bar") */
                       // in C#.
                       strcmp_call.match(skipCompilerGeneratedBoolification(
                           static_cast<E_unary const*>(e)->expr))&&
                       compared_to_pat.match(second_arg)) {
                VXCOV;
                myComparands.push_back(strcmpee);
            } else {
                VXCOV;
            }
        }
    }

    // Collect comparands under a conjunction/disjunction.
    void recursiveCollectComparands2(E_binary const *bny)
    {
        VXCOV;
        recursiveCollectComparands1(bny->e1);
        recursiveCollectComparands1(bny->e2);
    }


    bool myAllowComparisonToNonConstant;
    vector<Expression const *> myComparands;
    BinaryOp myParentOp;
};
