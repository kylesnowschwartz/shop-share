//
//  ErrorTypes.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 18/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

enum GRError {

    // Web socket error
    case textToDataError
    case incorrectDataFormatError
    case prematureDisconnection

    // JSON Errors
    case unableToParseJSON

    // Non-error errors
    case unimplementedMethod
}
