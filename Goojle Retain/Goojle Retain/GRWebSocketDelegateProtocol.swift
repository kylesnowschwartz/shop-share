//
//  GRWebSocketDelegateProtocol.swift
//  Goojle Retain
//
//  Created by Pragya on 18/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

protocol GRWebSocketDelegateProtocol {

    func connected()

    func send(_ action: ClientAction)

    func received(_ action: ServerAction)

    func receivedError(_ error: GRError)
}
