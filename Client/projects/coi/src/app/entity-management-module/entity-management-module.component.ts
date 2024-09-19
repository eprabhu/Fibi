import {Component, OnDestroy, OnInit} from '@angular/core';
import { AutoSaveService } from '../common/services/auto-save.service';
import { Subscription } from 'rxjs';
import {subscriptionHandler} from '../../../../fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../common/services/common.service';

@Component({
    selector: 'app-entity-management-module',
    templateUrl: './entity-management-module.component.html',
    styleUrls: ['./entity-management-module.component.scss']
})
export class EntityManagementModuleComponent implements OnInit, OnDestroy {

    result: any;
    dunsResponse: any;
    $subscriptions: Subscription[] = [];

    constructor(public _autoSaveService: AutoSaveService,
        private _commonService: CommonService) { }

    ngOnInit() {
        this._autoSaveService.initiateAutoSave();
    }

    triggerAutoSave() {
        this._autoSaveService.commonSaveTrigger$.next(true);
    }

    ngOnDestroy() {
        this._autoSaveService.stopAutoSaveEvent();
        this._commonService.isEntityModified = false;
        subscriptionHandler(this.$subscriptions);
    }

}
