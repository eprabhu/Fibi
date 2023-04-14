import { Component, EventEmitter, OnDestroy, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';

import { SfiService } from './sfi.service';
import { DataStoreService } from '../services/data-store.service';
import { CoiService } from '../services/coi.service';
import {subscriptionHandler} from "../../../../../fibi/src/app/common/utilities/subscription-handler";
import { Router } from '@angular/router';

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
    disclosureStatusCode: any;
    disclosureId: any;
    personId: any;
    isSFINotAvailable = false;

    constructor(
        private _sfiService: SfiService,
        private _dataStore: DataStoreService,
        public _coiService: CoiService,
        private _router: Router
    ) { }

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
        this.disclosureStatusCode = DATA.coiDisclosure.disclosureStatusCode;
        this.disclosureId =  DATA.coiDisclosure.disclosureId;
        this.isEditMode = this._dataStore.getEditModeForCOI();
        this.personId = DATA.coiDisclosure.personId;
        this.isSFINotAvailable = DATA.numberOfSFI === 0 && DATA.coiDisclosure.disclosureCategoryTypeCode == 3;
    }

    getSfiDetails() {
        this.$subscriptions.push(this._sfiService.getSfiDetails(this.disclosureId, this.disclosureStatusCode, this.personId).subscribe((data: any) => {
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

    openSfiDetails(condition: boolean, entityId: number) {
        this._router.navigate(['/coi/entity-details'], { queryParams: { entityId: '104',mode:'view' }});
    }

}
