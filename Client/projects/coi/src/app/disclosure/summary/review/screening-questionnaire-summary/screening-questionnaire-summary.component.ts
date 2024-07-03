import { Component, DoCheck, OnDestroy, OnInit } from '@angular/core';

import { environment } from '../../../../../environments/environment';
import { CommentConfiguration } from '../../../coi-interface';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';
import {CommonService} from "../../../../common/services/common.service";
import { Subscription } from 'rxjs';
import { CoiService } from '../../../services/coi.service';
import { subscriptionHandler } from '../../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { DataStoreService } from '../../../services/data-store.service';
import { coiReviewComment } from '../../../../shared-components/shared-interface';

@Component({
    selector: 'app-screening-questionnaire-summary',
    templateUrl: './screening-questionnaire-summary.component.html',
    styleUrls: ['./screening-questionnaire-summary.component.scss']
})
export class ScreeningQuestionnaireSummaryComponent implements OnInit, DoCheck, OnDestroy {

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
    isQuestionnaireCollapsed = false;
    $subscriptions: Subscription[] = [];
    activeQuestionnaire: any = null;


    constructor(
        private _commonService: CommonService,
        private _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,private _coiService: CoiService,private _dataStore: DataStoreService
    ) { }

    ngOnInit() {
        this.configuration.moduleItemKey = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
        this.commentConfiguration.disclosureId = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
        this.commentConfiguration.coiSectionsTypeCode = 1;
        this.configuration = Object.assign({}, this.configuration);
        this.listenToolKitFocusSection();
        setTimeout(() => {
            window.scrollTo(0,0);
        });
    }

    ngDoCheck() {
        const EXPORT_OPTIONS = document.querySelector('span.exportButton') as HTMLElement;
        if (EXPORT_OPTIONS) {
            EXPORT_OPTIONS.style.visibility = 'hidden';
        }
    }

    ngOnDestroy() {
     subscriptionHandler(this.$subscriptions);
    }

    modifyReviewComment() {
        let coiData = this._dataStore.getData();
        const disclosureDetails:coiReviewComment = {
            componentTypeCode: '4',
            documentOwnerPersonId: coiData.coiDisclosure.person.personId,
            subModuleItemKey: this.activeQuestionnaire?.QUESTIONNAIRE_ID,
            coiSubSectionsTitle: this.activeQuestionnaire?.QUESTIONNAIRE
        }
        this._commonService.$commentConfigurationDetails.next(disclosureDetails);
        this._coiService.isShowCommentNavBar = true;
    }

    setActiveQuestionnaire(event) {
        this.activeQuestionnaire = event;
    }

    //'COI801' parent element of ScreeningQuestionnaireSummaryComponent.
    private listenToolKitFocusSection() {
        this.$subscriptions.push(this._coiService.$isExpandSection.subscribe(ele => {
            if (ele.section == 'COI801') {
                this.isQuestionnaireCollapsed = !ele.isExpand;
            }
        }));
    }

}
