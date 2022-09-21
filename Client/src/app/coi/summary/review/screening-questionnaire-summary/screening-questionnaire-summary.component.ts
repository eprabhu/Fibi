import { Component, DoCheck, OnInit } from '@angular/core';

import { environment } from '../../../../../environments/environment';
import { CommonService } from '../../../../common/services/common.service';
import { CommentConfiguration } from '../../../coi-interface';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';

@Component({
    selector: 'app-screening-questionnaire-summary',
    templateUrl: './screening-questionnaire-summary.component.html',
    styleUrls: ['./screening-questionnaire-summary.component.css']
})
export class ScreeningQuestionnaireSummaryComponent implements OnInit, DoCheck {

    configuration: any = {
        moduleItemCode: 8,
        moduleSubitemCodes: [0],
        moduleItemKey: '',
        moduleSubItemKey: 0,
        actionUserId: this._commonService.getCurrentUserDetail('personID'),
        actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
        enableViewMode: true,
        isChangeWarning: true,
        isEnableVersion: true,
    };
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();

    constructor(
        private _commonService: CommonService,
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService
    ) { }

    ngOnInit() {
        this.configuration.moduleItemKey = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
        this.commentConfiguration.disclosureId = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
        this.commentConfiguration.coiSectionsTypeCode = 1;
        this.configuration = Object.assign({}, this.configuration);
    }

    ngDoCheck() {
        const EXPORT_OPTIONS = document.querySelector('span.exportButton') as HTMLElement;
        if (EXPORT_OPTIONS) {
            EXPORT_OPTIONS.style.visibility = 'hidden';
        }
    }

    modifyReviewComment() {
        this._dataStoreAndEventsService.modifyReviewComment(this.commentConfiguration);
    }

}
