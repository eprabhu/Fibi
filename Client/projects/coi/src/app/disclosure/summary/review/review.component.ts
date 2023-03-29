import { Component, OnInit } from '@angular/core';
import { CoiSummaryEventsAndStoreService } from '../coi-summary-events-and-store.service';

@Component({
    selector: 'app-coi-review',
    templateUrl: './review.component.html',
    styleUrls: ['./review.component.css']
})
export class ReviewComponent implements OnInit {

    isToolkitVisible = true;
    coiDetails: any = {};

    constructor(
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
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

}
