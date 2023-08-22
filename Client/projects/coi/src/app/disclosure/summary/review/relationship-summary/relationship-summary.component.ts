import { Component, Input, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';

import { environment } from '../../../../../environments/environment';
import { CommonService } from '../../../../common/services/common.service';
import { CommentConfiguration } from '../../../coi-interface';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';
import { CoiSummaryService } from '../../coi-summary.service';
import { HTTP_ERROR_STATUS } from "../../../../../../../fibi/src/app/app-constants";
import { DataStoreService } from '../../../services/data-store.service';
import { CoiService } from '../../../services/coi.service';

declare var $: any;

@Component({
    selector: 'app-relationship-summary',
    templateUrl: './relationship-summary.component.html',
    styleUrls: ['./relationship-summary.component.css']
})
export class RelationshipSummaryComponent implements OnInit {

    @Input() selectedProject: any;
    $subscriptions: Subscription[] = [];
    projectRelations: any = [];
    isOpenSlider = false;
    isShowHoverWhite = [];
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();

    reviewerConflict: any = { comment: {comments: ''}};
    projectDetails: any = {};
    conflictIndex = -1;
    coiDetails: any = {};
    conflictHistory: any = [];
    isCollapsed = true;
    entityDetails: any = null;
    isReadMore: boolean[] = [];
    projectConflictValidationMap = new Map();
    isShowNoDataCard = false;
    readMoreOrLess = false;

    constructor(
        private _coiSummaryService: CoiSummaryService,
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        public _commonService: CommonService,
        private _dataStore: DataStoreService,
        private _coiService: CoiService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.getEntityProjectRelations();
        this.listenForCoiDetails();
        this.commentConfiguration.disclosureId = this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId;
        this.commentConfiguration.coiSectionsTypeCode = 3;
    }

    private fetchCOIDetails(): void {
        this.coiDetails = this._dataStoreAndEventsService.getData(
            this._dataStoreAndEventsService.coiSummaryConfig.currentDisclosureId,
            ['coiDisclosure']
        ).coiDisclosure;
    }

    setSubSectionList() {
        this.commentConfiguration.subSectionList = this.projectRelations.map(ele => {
            return {
                coiSubSectionsCode: ele.disclosureDetailsId,
                description: ele.coiFinancialEntity.coiEntity.coiEntityName,
                coiDisclosureDetails: ele
            };
        });
    }

    private listenForCoiDetails() {
        this.$subscriptions.push(this._dataStoreAndEventsService.dataEvent.subscribe((data: any) => {
            if (data) {
                // this.selectedProject = data;
                this.fetchCOIDetails();
            }
        }));
    }

    getEntityProjectRelations() {
        this.isShowNoDataCard = false;
        this.$subscriptions.push(
            this._coiSummaryService.getEntityProjectRelations(this.selectedProject.moduleCode, this.selectedProject.moduleItemId,
               Number(this.coiDetails.disclosureId), this.coiDetails.disclosureStatusCode, this.coiDetails.personId)
                .subscribe((data: any) => {
                if (data && data.length > 0) {
                    this.isShowNoDataCard = true;
                    this.projectRelations = data;
                }
        }, _err => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching project details. Please try again.');
        }));
    }

    closePage(event) {
        this.isOpenSlider = false;
        this.entityDetails = null;
        this.getEntityProjectRelations();
        if (event) {
            this.updateDisclosureConflictStatus(event);
        }
    }

    setEntityDetails(entity, index) {
        this.entityDetails = {
            disclosureDetailsId: entity.disclosureDetailsId,
            entityName: entity.personEntity?.coiEntity?.entityName,
            index: index,
            personId: this.coiDetails.personId,
            moduleItemKey: this.selectedProject.moduleItemKey,
            moduleItemId: this.selectedProject.moduleItemId,
            moduleCode: this.selectedProject.moduleCode,
            title: this.selectedProject.title,
            coiProjConflictStatusType: entity.coiProjConflictStatusType,
            disclosureId: this.coiDetails.disclosureId,
            comment:entity.disclComment.comment,
            sponsor: this.selectedProject.sponsor,
            primeSponsor: this.selectedProject.primeSponsor
        }
    }

    private updateDisclosureConflictStatus(status): void {
        this.coiDetails.coiConflictStatusType = status;
        this.coiDetails.conflictStatusCode = status.conflictStatusCode;
        this._dataStore.updateStore(['coiDisclosure'], { coiDisclosure: this.coiDetails });
    }

    // function for opening the slider in relationship section common comment
    private openReviewComment(index) {
        this._coiService.isShowCommentNavBar = !this._coiService.isShowCommentNavBar;
    }

    // function for opening the slider in SFI listing table section comment (relationship section)
    private openCommoncommentSlider() {
        this._coiService.isShowCommentNavBar = !this._coiService.isShowCommentNavBar;
    }

}
