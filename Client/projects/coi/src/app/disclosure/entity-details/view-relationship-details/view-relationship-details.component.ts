import { Component, Input, OnChanges, OnDestroy } from '@angular/core';
import { EntityDetailsService } from '../entity-details.service';
import { ActivatedRoute, Router } from '@angular/router';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription, forkJoin } from 'rxjs';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { DATE_PLACEHOLDER } from '../../../../../../fibi/src/app/app-constants';
import { compareDates, parseDateWithoutTimestamp } from '../../../../../../fibi/src/app/common/utilities/date-utilities';
import { slideInOut } from '../../../../../../fibi/src/app/common/utilities/animations';
import { NavigationService } from '../../../common/services/navigation.service';

@Component({
    selector: 'app-view-relationship-details',
    templateUrl: './view-relationship-details.component.html',
    styleUrls: ['./view-relationship-details.component.scss'],
    animations: [slideInOut]
})
export class ViewRelationshipDetailsComponent implements OnDestroy, OnChanges {


    relationshipsDetails: any = {};
    $subscriptions: Subscription[] = [];
    personEntityRelationships: any = [];
    @Input() updateRelationshipDetails: any;
    @Input() entityId: any;
    isReadMoreBusinessArea = false;
    isReadMoreUniversity = false;
    isReadMoreRelationWith = false;
    sfiStatus = '';
    additionalDetails: any = {
        sponsorsResearch: false
    };
    datePlaceHolder = DATE_PLACEHOLDER;
    mandatoryList = new Map();
    @Input() isEditMode = false;
    isQuestionnaireCompleted = false;
    @Input() isTriggeredFromSlider = false;
    isEnableActivateInactivateSfiModal = false;
    allRelationQuestionnaires = [];
    @Input() entityDetails: any = {};
    isHoverCardViewMore = false;
    hasUserExpanded = false;
    isFinalizeApi = false;
    isChangesInFieldValue = false;
    isRelationshipActive = false;
    @Input() onDeleteTimestamp = null;


    constructor(public entityDetailsServices: EntityDetailsService, private _router: Router,
        private _route: ActivatedRoute, public commonService: CommonService, private _navigationService: NavigationService) {
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    async ngOnChanges() {
        await this.getEntityDetails(this.getEntityId());
        if (!this.isEditMode) {
            this.isQuestionnaireCompleted = true;
        }
        if (this.updateRelationshipDetails?.length) {
            this.relationshipsDetails.updateTimestamp = this.updateRelationshipDetails[0].updateTimestamp;
            this.updateRelationshipDetails.forEach(element => {
                this.personEntityRelationships.push(element);
            });
        }
        if (this.isEditMode) {
            this.getQuestionnaire();
        }
        this.listenForQuestionnaireSave();
        this.scrollEvent();
        if (this.onDeleteTimestamp) {
            this.relationshipsDetails.updateTimestamp = this.onDeleteTimestamp;
        }
    }

    getEntityId() {
        return this._route.snapshot.queryParamMap.get('personEntityId') || this.entityId;
    }

    getEntityDetails(personEntityId) {
        return new Promise<boolean>((resolve) => {
            this.$subscriptions.push(this.entityDetailsServices.getRelationshipEntityDetails(personEntityId).subscribe((res: any) => {
                this.relationshipsDetails = res.personEntity;
                this.personEntityRelationships = res.personEntityRelationships;
                this.setAdditionalDetails(res.personEntity);
                this.entityDetailsServices.canMangeSfi = this.relationshipsDetails.personId === this.commonService.currentUserDetails.personId ? true : false;
                this.sfiStatus = this.getSfiStatus();
                this.isRelationshipActive = this.sfiStatus === 'Draft' || this.sfiStatus === 'Inactive' ? false : true;
                resolve(true);
            }, _error => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                resolve(false);

            }));
        });
    }

    navigateBack() {
        if (this._navigationService.previousURL.includes('personEntityId') || this._navigationService.previousURL.includes('entity-management') ||
            this._navigationService.previousURL.includes('create-sfi/create') || this._navigationService.previousURL === '') {
            this._router.navigate(['/coi/user-dashboard/entities']);
        } else {
            this._router.navigateByUrl(this._navigationService.previousURL);
        }
    }

    getSfiStatus(): string {
        if (this.relationshipsDetails.isRelationshipActive && (this.relationshipsDetails.versionStatus === 'ACTIVE' ||
             this.relationshipsDetails.versionStatus === 'ARCHIVE')) {
            return 'Active';
        } else if (!this.relationshipsDetails.isRelationshipActive && this.relationshipsDetails.versionStatus === 'ACTIVE') {
            return 'Inactive';
        } else if (this.relationshipsDetails.versionStatus === 'PENDING') {
            return 'Draft';
        }
    }

    private checkMandatoryFilled(): boolean {
        this.mandatoryList.clear();
        if (!this.additionalDetails.involvementStartDate) {
            this.mandatoryList.set('date', 'Please enter a start date.');
        }
        if (!this.additionalDetails.staffInvolvement) {
            this.mandatoryList.set('staff', 'Please enter Relationship with Entity details.');
        }
        if (!this.additionalDetails.studentInvolvement) {
            this.mandatoryList.set('student', 'Please enter Principle Business Area of Entity details.');
        }
        if (!this.additionalDetails.instituteResourceInvolvement) {
            this.mandatoryList.set('resource', 'Please enter Relationship of Entity to your University responsibilities details.');
        }
        this.endDateValidation();
        return this.mandatoryList.size !== 0 ? false : true;
    }

    endDateValidation(): void {
        this.mandatoryList.delete('endDate');
        if (this.additionalDetails.involvementStartDate && this.additionalDetails.involvementEndDate &&
            (compareDates(this.additionalDetails.involvementStartDate, this.additionalDetails.involvementEndDate) === 1)) {
            this.mandatoryList.set('endDate', 'Please provide a valid date.');
        }
    }

    setAdditionalDetails(details) {
        this.additionalDetails.involvementStartDate = parseDateWithoutTimestamp(details.involvementStartDate);
        this.additionalDetails.involvementEndDate = parseDateWithoutTimestamp(details.involvementEndDate);
        this.additionalDetails.sponsorsResearch = details.sponsorsResearch;
        this.additionalDetails.instituteResourceInvolvement = details.instituteResourceInvolvement;
        this.additionalDetails.studentInvolvement = details.studentInvolvement;
        this.additionalDetails.staffInvolvement = details.staffInvolvement;
        this.additionalDetails.personEntityId = this.entityId;
    }

    activateInactivateSfi() {
        if ((this.isQuestionnaireCompleted && (this.isChangesInFieldValue||
            this.entityDetailsServices.isRelationshipQuestionnaireChanged))) {
            this.saveRelationship();
        }
        this.isFinalizeApi = this.sfiStatus === 'Draft' ? true : false;
        this.isEnableActivateInactivateSfiModal = true;
    }

    saveRelationship() {
        if (this.checkMandatoryFilled() && this.isChangesInFieldValue) {
            this.updatePersonEntityAdditionalDetails();
        }
        this.entityDetailsServices.globalSave$.next();
        if (this.entityDetailsServices.isRelationshipQuestionnaireChanged) {
            this.entityDetailsServices.isRelationshipQuestionnaireChanged = false;
        }
    }

    goToHome() {
        this._router.navigate(['/coi/user-dashboard/entities']);
    }

    closeActivateInactivateSfiModal(event) {
        if (event) {
            this.relationshipsDetails.isRelationshipActive = event.isRelationshipActive;
            if (event.versionStatus) {
                this.relationshipsDetails.versionStatus = event.versionStatus;
                if (this.isQuestionnaireCompleted) {
                    this._router.navigate(['/coi/entity-details/entity'],
                        { queryParams: { personEntityId: event.personEntityId, mode: 'view' } });
                }
            }
            this.relationshipsDetails.updateTimestamp = event.updateTimestamp;
            this.sfiStatus = this.getSfiStatus();
            this.isRelationshipActive = this.sfiStatus === 'Draft' || this.sfiStatus === 'Inactive' ? false : true;
            this.isEnableActivateInactivateSfiModal = false;
            if (this.isFinalizeApi) {
                this.isFinalizeApi = false;
            }
            if (this.entityId !== event.personEntityId) {
                this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: event.personEntityId, mode: 'view' } });
            }
        } else {
            this.isEnableActivateInactivateSfiModal = false;
        }
    }

    getQuestionnaire() {
        const QUEST_REQ_OBJ_LIST = [];
        this.personEntityRelationships.forEach(rel => {
            this.setQuestionnaireRequestObject(rel.validPersonEntityRelTypeCode, QUEST_REQ_OBJ_LIST);
        });
        this.checkQuestionnaireCompleted(QUEST_REQ_OBJ_LIST);
    }

    setQuestionnaireRequestObject(subItemCode, list) {
        list.push(this.getApplicableQuestionnaire({
            moduleItemCode: 8,
            moduleSubItemCode: 801,
            moduleSubItemKey: subItemCode,
            moduleItemKey: this.entityId,
            actionUserId: this.commonService.getCurrentUserDetail('personId'),
            actionPersonName: this.commonService.getCurrentUserDetail('fullName'),
            questionnaireNumbers: [],
            questionnaireMode: 'ACTIVE_ANSWERED_UNANSWERED'
        }));
    }

    getApplicableQuestionnaire(requestObject) {
        requestObject = JSON.parse(JSON.stringify(requestObject));
        return this.entityDetailsServices.getApplicableQuestionnaire(requestObject);
    }

    checkQuestionnaireCompleted(questionList) {
        this.$subscriptions.push(forkJoin(...questionList).subscribe(data => {
            this.allRelationQuestionnaires = [];
            data.forEach((d: any) => this.combineQuestionnaireList(d.applicableQuestionnaire));
            this.isQuestionnaireCompleted = this.isAllQuestionnaireCompleted(this.allRelationQuestionnaires);
        }, err => {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    combineQuestionnaireList(newList) {
        this.allRelationQuestionnaires = [...this.allRelationQuestionnaires, ...newList];
    }

    isAllQuestionnaireCompleted(questionnaireList) {
        return questionnaireList.every(questionnaire => questionnaire.QUESTIONNAIRE_COMPLETED_FLAG === 'Y');
    }

    updatePersonEntityAdditionalDetails() {
        this.$subscriptions.push(this.entityDetailsServices.updateAdditionalDetails(this.additionalDetails).subscribe((res: any) => {
            this.isChangesInFieldValue = false;
            this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Significant Financial Interest updated successfully completed.');
            this.relationshipsDetails.updateTimestamp = res.updateTimestamp;
        }, error => {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    listenForQuestionnaireSave() {
        this.$subscriptions.push(this.entityDetailsServices.$saveQuestionnaireAction.subscribe((params: any) => {
            if (params) {
                this.getQuestionnaire();
                this.entityDetailsServices.isRelationshipQuestionnaireChanged = false;
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Relationship saved successfully `);
            } else {
                this.commonService.showToast(HTTP_ERROR_STATUS, `Error in saving relationship`);
            }
        }));
    }

    viewEntityDetails() {
        this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: this.entityDetails.entityId } });
    }

    redirectUrl(url) {
        if (url.includes('http')) {
            window.open(url, '_blank');
        } else {
            window.open('//' + url, '_blank');
        }
    }

    modifySfi() {
        this.$subscriptions.push(this.entityDetailsServices.modifyPersonEntity({ personEntityId: this.getEntityId() }).subscribe((res: any) => {
            this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: res.personEntityId, mode: 'edit' } });
        }));
    }

    /**
     * keep track of window scroll offset
     */
    scrollEvent() {
        this.$subscriptions.push(this.commonService.onScrollEvent().subscribe((res) => {
            if (!this.hasUserExpanded) {
                const scrollHeight = res.pageYOffset;
                if (this.isEditMode && (scrollHeight > 250)) {
                    this.entityDetailsServices.isExpanded = false;
                }
                if (!this.isEditMode && scrollHeight > 130) {
                    this.entityDetailsServices.isExpanded = false;
                    this.isReadMoreUniversity = false;
                    this.isReadMoreBusinessArea = false;
                    this.isReadMoreRelationWith = false;
                } else if (scrollHeight === 0) {
                    this.entityDetailsServices.isExpanded = true;
                }
            }
        }));
    }
}

