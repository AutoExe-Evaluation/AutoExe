// Constraint Solver related functions

#ifndef CONSTRAINT_SOLVER_H
#define CONSTRAINT_SOLVER_H

#include <string>
#include <string_view>
#include <vector>

#include "z3++.h"

#include "common.h"

inline std::string llm_model;
inline std::vector<std::pair<std::string, z3::check_result>> LLM_QUERIES;

/**
 * Check a constraint using Z3
 *
 * @param formula    The constraint to check
 * @param ctx        Z3 context to be used
 * @param timeout_ms Timeout for Z3 in milliseconds
 * @param tries      Number of tries permitted
 * @return           The result of the check
 */
[[nodiscard]] z3::check_result check_z3(
    const z3::expr& formula, z3::context& ctx,
    unsigned int timeout_ms = 500, unsigned int tries = 10
);

/**
 * Check a constraint using LLM
 *
 * @param slice      Current code slice
 * @return           The result of the check
 */
[[nodiscard]] z3::check_result check_llm(std::string_view slice);

/**
 * Check a constraint is unsatisfiable using Z3
 *
 * @param formula    The constraint to check
 * @param ctx        Z3 context to be used
 * @param timeout_ms Timeout for Z3 in milliseconds
 * @param tries      Number of tries permitted
 * @return           True if the formula is guaranteed to be unsatisfiable
 */
[[nodiscard]] inline bool is_unsat_z3(
    const z3::expr& formula, z3::context& ctx,
    unsigned int timeout_ms = 500, unsigned int tries = 10
)
{
    if (formula.is_true()) return false;
    if (formula.is_false()) return true;
    return check_z3(formula, ctx, timeout_ms, tries) == z3::unsat;
}

/**
 * Check a constraint is unsatisfiable using Z3
 *
 * @param formula    The constraint to check
 * @param ctx        Z3 context to be used
 * @param slice      Current code slice
 * @return           True if the formula is guaranteed to be unsatisfiable
 */
[[nodiscard]] inline bool is_unsat(const z3::expr& formula, z3::context& ctx, std::string_view slice)
{
    if (formula.is_true()) return false;
    if (formula.is_false()) return true;
    return check_llm(slice) == z3::unsat;
}

/**
 * Check multiple constraints using Z3 (connected using AND)
 *
 * @param  formula1   The first constraint
 * @param  formula    The constraints to check
 * @tparam timeout_ms Timeout for Z3 in milliseconds
 * @tparam tries      Number of tries permitted
 * @return            The result of the check
 */
template<typename... Args, int timeout_ms = 600, int tries = 50>
[[nodiscard]] z3::check_result check_z3_and(const z3::expr& formula1, const Args&... formula)
{
    return check_z3((formula1 && ... && formula), timeout_ms, tries);
}

/**
 * Check multiple constraints using Z3 (connected using OR)
 *
 * @param  formula1   The first constraint
 * @param  formula    The constraints to check
 * @tparam timeout_ms Timeout for Z3 in milliseconds
 * @tparam tries      Number of tries permitted
 * @return            The result of the check
 */
template<typename... Args, int timeout_ms = 600, int tries = 50>
[[nodiscard]] z3::check_result check_z3_or(const z3::expr& formula1, const Args&... formula)
{
    return check_z3((formula1 || ... || formula), timeout_ms, tries);
}

/**
 * Make a Z3 sequence
 *
 * @param name Name of the sequence
 * @param ctx  Z3 context to be used
 * @return     An expr referencing a sequence
 */
[[nodiscard]] z3::expr z3_sequence(std::string_view name, z3::context& ctx);

/**
 * Make several Z3 sequences
 *
 * @param names Names of the sequences
 * @param ctx   Z3 context to be used
 * @return      A list of expr referencing the sequences
 */
[[nodiscard]] std::vector<z3::expr> z3_sequences(const std::vector<std::string>& names, z3::context& ctx);

/**
 * Make several Z3 sequences
 *
 * @param names Names of the sequences delimited by whitespaces
 * @param ctx   Z3 context to be used
 * @return      A list of expr referencing the sequences
 */
[[nodiscard]] std::vector<z3::expr> z3_sequences(const std::string& names, z3::context& ctx);

#endif //CONSTRAINT_SOLVER_H
