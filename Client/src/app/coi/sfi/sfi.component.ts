import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';

import { SfiService } from './sfi.service';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { DataStoreService } from '../services/data-store.service';
import { CoiService } from '../services/coi.service';


@Component({
    selector: 'app-sfi',
    templateUrl: './sfi.component.html',
    styleUrls: ['./sfi.component.css']
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
        public _coiService: CoiService
    ) { }

    ngOnInit() {
        this._coiService.isShowSFIInfo = true;
        this.getEditMode();
        this.getSfiDetails();
        this.listenDataChangeFromStore();
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

    closeSFIInfo() {
        this._coiService.isShowSFIInfo = false;
    }

}
