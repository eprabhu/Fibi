import { Component, Input, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';

import { environment } from '../../../../../environments/environment';
import { CommonService } from '../../../../common/services/common.service';
import { CommentConfiguration } from '../../../coi-interface';
import { CoiSummaryEventsAndStoreService } from '../../coi-summary-events-and-store.service';
import { CoiSummaryService } from '../../coi-summary.service';

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
    // selectedProject: any = {};
    deployMap = environment.deployUrl;
    commentConfiguration: CommentConfiguration = new CommentConfiguration();

    reviewerConflict: any = { comment: {comments: ''}};
    projectDetails: any = {};
    conflictIndex = -1;
    coiDetails: any = {};
    conflictHistory: any = [];

    projectConflictValidationMap = new Map();

    constructor(
        private _coiSummaryService: CoiSummaryService,
        public _dataStoreAndEventsService: CoiSummaryEventsAndStoreService,
        private _commonService: CommonService
    ) { }

    ngOnInit() {
        this.fetchCOIDetails();
        this.getEntityProjectRelations();
        // this.listenForProjectDetails();
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

    private listenForProjectDetails() {
        this.$subscriptions.push(this._dataStoreAndEventsService.$projectDetails.subscribe((data: any) => {
            if (data) {
                this.selectedProject = data;
                this.getEntityProjectRelations();
            }
        }));
    }

    getEntityProjectRelations() {
        this.$subscriptions.push(
            this._coiSummaryService.getEntityProjectRelations(this.selectedProject.moduleCode, this.selectedProject.moduleItemId,
               Number(this.coiDetails.disclosureId), this.coiDetails.disclosureStatusCode).subscribe((data: any) => {
                if (data && data.coiDisclosureDetails?.length > 0) {
                    this.projectRelations = data.coiDisclosureDetails;
                }
                // if (data && this.selectedProject.proposalIdlinkedInDisclosure && data.proposals.length) {
                //     this.selectedProject = data.proposals[0];
                // }
            // this.setSubSectionList();
        }, _err => {
            //this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching project details. Please try again.');
        }));
    }

    modifyReviewComment(isSubSectionComment = false, subSectionCode = null) {
        this.commentConfiguration.isSubSectionComment = isSubSectionComment;
        this.commentConfiguration.coiSubSectionsId = subSectionCode;
        this._dataStoreAndEventsService.modifyReviewComment(this.commentConfiguration);
    }

    projectConflictValidation() {
        this.projectConflictValidationMap.clear();
        if (this.reviewerConflict.coiReviewerStatusCode === 'null' || !this.reviewerConflict.coiReviewerStatusCode) {
            this.projectConflictValidationMap.set('coiReviewerStatusCode', 'Please select conflict status.');
        }
        if (!this.reviewerConflict.comment.comments) {
            this.projectConflictValidationMap.set('comment', 'Please add a comment.');
        }
        return this.projectConflictValidationMap.size === 0 ? true : false;
    }

    updateProjectConflictStatus() {
        if (this.projectConflictValidation()) {
            this.reviewerConflict.disclosureDetailsId = this.projectDetails.disclosureDetailsId;
            this.$subscriptions.push(
                this._coiSummaryService.updateProjectConflictStatus({
                    coiDisclosureDetail: this.reviewerConflict
                }).subscribe((data: any) => {
                    this.projectRelations[this.conflictIndex].coiReviewerStatusCode = data.coiReviewerStatusCode;
                    this.projectRelations[this.conflictIndex].coiReviewerStatus = data.coiReviewerStatus;
                   // this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Conflict status updated successfully.');
                    $('#reviewer-conflict-modal').modal('hide');
                    this.clearConflictModal();
                }, _err => {
                   // this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating conflict status. Please try again.');
                }));
        }
    }

    loadProjectConflictHistory(disclosureDetailsId) {
        this.$subscriptions.push(
            this._coiSummaryService.loadProjectConflictHistory(disclosureDetailsId).subscribe((data: any) => {
                this.conflictHistory = data;
                $('#conflict-history-modal').modal('show');
            }, _err => {
                //this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in fetching conflict status history. Please try again.');
            }));
    }

    clearConflictModal() {
        this.projectConflictValidationMap.clear();
        this.reviewerConflict = { comment: {comments: ''}};
    }

    closePage() {
      this.isOpenSlider = false;
    }

}
