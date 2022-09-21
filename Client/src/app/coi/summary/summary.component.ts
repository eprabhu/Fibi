import { Component, OnInit } from '@angular/core';

import { DataStoreService } from '../services/data-store.service';
import { CoiSummaryEventsAndStoreService } from './coi-summary-events-and-store.service';

@Component({
    selector: 'app-summary',
    templateUrl: './summary.component.html',
    styleUrls: ['./summary.component.css']
})
export class SummaryComponent implements OnInit {

    constructor(
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _dataStore: DataStoreService
    ) { }

    ngOnInit() {
        this.getCOIDetails();
    }

    getCOIDetails(): void {
        const DATA = this._dataStore.getData();
        this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId = DATA.coiDisclosure.disclosureId;
        this._dataStoreAndEventsService.setStoreData(DATA, DATA.coiDisclosure.disclosureId);
    }

}
