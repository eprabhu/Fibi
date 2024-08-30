import {Component, OnDestroy, OnInit} from '@angular/core';
import { AutoSaveService } from '../common/services/auto-save.service';
import { EntityManagementService } from './entity-management.service';
import { EntityDataStoreService } from './entity-data-store.service';
import { Subscription } from 'rxjs';
import {subscriptionHandler} from '../../../../fibi/src/app/common/utilities/subscription-handler';

@Component({
    selector: 'app-entity-management-module',
    templateUrl: './entity-management-module.component.html',
    styleUrls: ['./entity-management-module.component.scss']
})
export class EntityManagementModuleComponent implements OnInit, OnDestroy {

    result: any;
    dunsResponse: any;
    $subscriptions: Subscription[] = [];

    constructor(public _autoSaveService: AutoSaveService, private _entityManagementService: EntityManagementService, private _dataStore: EntityDataStoreService) { }

    ngOnInit() {
        this._autoSaveService.initiateAutoSave();
        // this.fetchRisk();
    }

    triggerAutoSave() {
        this._autoSaveService.commonSaveTrigger$.next(true);
    }

    ngOnDestroy() {
        this._autoSaveService.stopAutoSaveEvent();
        subscriptionHandler(this.$subscriptions);
    }

}
