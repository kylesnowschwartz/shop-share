//
//  DataSource.swift
//  Goojle Retain
//
//  Created by Timothy Barraclough on 18/08/17.
//  Copyright © 2017 Shop Share Industries. All rights reserved.
//

import Foundation

protocol DataSourceDelegate {

    func updated(_ lists: [List])

}

protocol DataSourceProtocol {

    var delegate : DataSourceDelegate? { get set }

    func requestLists()

}

class DataSource : DataSourceProtocol, ListObserverProtocol {

    func didConnectToWebSocket() {
        requestLists()
    }

    var delegate: DataSourceDelegate?

    func listsUpdated(_ lists: [List]) {
        delegate?.updated(lists)
    }

    init(_ delegate : DataSourceDelegate?) {
        self.delegate = delegate
        GRWebSocket.shared.delegate.observer = self

    }

    func requestLists() {
        GRWebSocket.shared.send(.getLists)
    }

}
