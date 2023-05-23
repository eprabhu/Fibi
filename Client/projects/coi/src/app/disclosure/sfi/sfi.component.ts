import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';

import { SfiService } from './sfi.service';
import { DataStoreService } from '../services/data-store.service';
import { CoiService } from '../services/coi.service';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { NavigationEnd, Router } from '@angular/router';
import { CommonService } from '../../common/services/common.service';

@Component({
    selector: 'app-sfi',
    templateUrl: './sfi.component.html',
    styleUrls: ['./sfi.component.scss']
})
export class SfiComponent implements OnInit, OnDestroy {
    $subscriptions: Subscription[] = [];
    coiFinancialEntityDetails: any[] = [];
    searchText: string;
    dependencies = ['coiDisclosure', 'numberOfSFI'];
    isEditMode = false;
    conflictStatusCode: any;
    disclosureId: any;
    personId: any;
    isSFINotAvailable = false;
    reviewStatus: any;

    constructor(
        private _sfiService: SfiService,
        private _dataStore: DataStoreService,
        public _coiService: CoiService,
        private _router: Router) {
    }

    ngOnInit() {
        this._coiService.isShowSFIInfo = true;
        this.getEditMode();
        this.getSfiDetails();
        this.listenDataChangeFromStore();
        this.listenForAdd();
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    getEditMode() {
        const DATA = this._dataStore.getData(this.dependencies);
        this.conflictStatusCode = 0;
        this.conflictStatusCode = DATA.coiDisclosure.conflictStatusCode;
        this.reviewStatus = DATA.coiDisclosure.reviewStatusCode;
        this.disclosureId =  DATA.coiDisclosure.disclosureId;
        this.isEditMode = this._dataStore.getEditModeForCOI();
        this.personId = DATA.coiDisclosure.personId;
        this.isSFINotAvailable = DATA.numberOfSFI === 0 && DATA.coiDisclosure.disclosureCategoryTypeCode == 3;
    }

    getSfiDetails() {
        this.$subscriptions.push(this._sfiService.getSfiDetails(this.disclosureId, this.reviewStatus, this.personId).subscribe((data: any) => {
            if (data) {
                this.coiFinancialEntityDetails = data;
            }
        }));
    }

    listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                if (dependencies.some((dep) => this.dependencies.includes(dep))) {
                    this.getEditMode();
                }
            })
        );
    }

    listenForAdd() {
        this.$subscriptions.push(
            this._sfiService.$addSfi.subscribe((data: boolean) => {
                this.getSfiDetails();
                this._sfiService.isShowSfiNavBar = false;
                this.removeEntityId();
            })
        );
    }

    removeEntityId() {
        this._router.navigate([], {
          queryParams: {entityId: null},
          queryParamsHandling: 'merge'
        })
      }

    closeSFIInfo() {
        this._coiService.isShowSFIInfo = false;
    }

}
