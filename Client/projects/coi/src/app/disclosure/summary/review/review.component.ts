import { Component, OnInit } from '@angular/core';
import { CoiSummaryEventsAndStoreService } from '../coi-summary-events-and-store.service';
import { Subscription } from 'rxjs';
import { DataStoreService } from '../../services/data-store.service';

@Component({
    selector: 'app-coi-review',
    templateUrl: './review.component.html',
    styleUrls: ['./review.component.css']
})
export class ReviewComponent implements OnInit {

    isToolkitVisible = true;
    coiDetails: any = {};
    isRelationCollapsed = true;
    $subscriptions: Subscription[] = [];

    constructor(
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _dataStore: DataStoreService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.listenDataChangeFromStore();
    }

    fetchCOIDetails(): void {
        this.coiDetails = this._dataStoreAndEventsService.getData(
            this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId,
            ['coiDisclosure']
        ).coiDisclosure;
    }

    toggleToolkitVisibility(): void {
        this.isToolkitVisible = !this.isToolkitVisible;
        this._dataStoreAndEventsService.$isToolkitVisible.next(this.isToolkitVisible);
    }
    
    private listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
                this.fetchCOIDetails();
            })
        );
    }

}
