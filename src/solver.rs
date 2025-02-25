use z3;
use z3::ast::Bool;

/// Given a context, a main equation and a list of
/// filtered equations, solve for the main equation
/// # Arguments
/// * `ctx` - The Z3 context
/// * `main_equation` - The main equation to solve for
/// * `filtered` - The list of filtered equations
/// # Returns
/// * `SatResult` - The result of the solving
pub fn solve<'a>(
    ctx: &'a z3::Context,
    main_equation: &Option<Bool>,
    filtered: &Vec<Bool>,
) -> Option<z3::Model<'a>> {
    let solver = z3::Solver::new(ctx);
    let possible = find_possible_equations(ctx, main_equation, filtered);
    if !possible.is_empty() {
        for p in possible {
            solver.assert(&p);
        }
    }
    if let Some(eq) = main_equation {
        solver.assert(eq);
    }
    let result = solver.check();
    let model = match result {
        z3::SatResult::Sat => solver.get_model(),
        _ => None,
    };
    assert_eq!(result, z3::SatResult::Sat);
    model
}

pub fn model_to_features(model: &Option<z3::Model>) -> (Vec<String>, Vec<String>) {
    if model.is_none() {
        return (Vec::new(), Vec::new());
    }
    let model = model.as_ref().unwrap();
    (
        model_to_enabled_features(model),
        model_to_disabled_features(model),
    )
}

fn model_to_enabled_features(model: &z3::Model) -> Vec<String> {
    let mut features: Vec<String> = Vec::new();

    for it in model.iter() {
        let name = it.name().to_string();

        if let Some(true) = model
            .eval(&it.apply(&[]), true)
            .and_then(|v| v.as_bool())
            .and_then(|v| v.as_bool())
        {
            features.push(name);
        }
    }

    features
}

fn model_to_disabled_features(model: &z3::Model) -> Vec<String> {
    let mut features: Vec<String> = Vec::new();

    for it in model.iter() {
        let name = it.name().to_string();

        if let Some(false) = model
            .eval(&it.apply(&[]), true)
            .and_then(|v| v.as_bool())
            .and_then(|v| v.as_bool())
        {
            features.push(name);
        }
    }
    features
}

fn find_possible_equations<'a>(
    ctx: &z3::Context,
    main_equation: &Option<Bool>,
    filtered: &Vec<Bool<'a>>,
) -> Vec<Bool<'a>> {
    let mut possible: Vec<Bool<'a>> = Vec::new();
    let solver = z3::Solver::new(ctx);
    for eq in filtered {
        if let Some(eq) = main_equation {
            solver.assert(eq);
        }
        if !possible.is_empty() {
            for p in &possible {
                solver.assert(p);
            }
        }
        solver.assert(eq);
        let result = solver.check();
        if result == z3::SatResult::Sat {
            possible.push(eq.clone());
            continue;
        }
        solver.reset();
    }
    possible
}
