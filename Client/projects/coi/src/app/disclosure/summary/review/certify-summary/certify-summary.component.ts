import { Component, OnInit } from '@angular/core';

import { environment } from '../../../../../environments/environment';
import { CommentConfiguration } from '../../../coi-interface';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';

@Component({
    selector: 'app-certify-summary',
    templateUrl: './certify-summary.component.html',
    styleUrls: ['./certify-summary.component.css']
})
export class CertifySummaryComponent implements OnInit {

    coiDetails: any = {};
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();

    constructor(
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.commentConfiguration.disclosureId = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
        this.commentConfiguration.coiSectionsTypeCode = 4;
    }

    private fetchCOIDetails(): void {
        this.coiDetails = this._dataStoreAndEventsService.getData(
            this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId,
            ['coiDisclosure']
        ).coiDisclosure;
    }

    modifyReviewComment() {
        this._dataStoreAndEventsService.modifyReviewComment(this.commentConfiguration);
    }

}
