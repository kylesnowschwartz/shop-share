//
//  Result.swift
//  Goojle Retain
//
//  Created by Pragya on 18/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

enum Result<A> {
    case success(A)
    case error(GRError)

    func map<B>(_ f: (A) -> B) -> Result<B> {
        switch self {
        case .success(let a):
            return .success(f(a))
        case .error(let error):
            return .error(error)
        }
    }
}

