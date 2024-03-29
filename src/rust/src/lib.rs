// Copyright 2021 by the authors.
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

use extendr_api::prelude::*;

pub mod tuples;

#[extendr]
fn r_all_tuples(n: u16, sigma: Vec<String>) -> Vec<String> {
    tuples::all_tuples(n, &sigma)
}

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod gcatbase; // like R package name
    fn r_all_tuples;
}
