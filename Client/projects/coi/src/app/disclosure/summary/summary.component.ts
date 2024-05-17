import { Component, OnInit } from '@angular/core';

import { DataStoreService } from '../services/data-store.service';
import { CoiSummaryEventsAndStoreService } from './coi-summary-events-and-store.service';
import { Subscription } from 'rxjs';

@Component({
    selector: 'app-summary',
    templateUrl: './summary.component.html',
    styleUrls: ['./summary.component.scss']
})
export class SummaryComponent implements OnInit {
   
    $subscriptions: Subscription[] = [];

    constructor(
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _dataStore: DataStoreService
    ) { }

    ngOnInit() {
        this.getCOIDetails();
        this.listenDataChangeFromStore();
        window.scrollTo(0,0);
    }
    
    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.getCOIDetails();
                this._dataStoreAndEventsService.dataEvent.next(['coiDisclosure']);
            })
        );
    }

    getCOIDetails(): void {
        const DATA = this._dataStore.getData();
        this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId = DATA.coiDisclosure.disclosureId;
        this._dataStoreAndEventsService.setStoreData(DATA, DATA.coiDisclosure.disclosureId);
    }

}
