import { Component, EventEmitter, Input, OnDestroy, Output } from '@angular/core';
import { EntityDetailsService } from '../entity-details.service';
import { ActivatedRoute, Router } from '@angular/router';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subscription, forkJoin } from 'rxjs';
import { ADMIN_DASHBOARD_URL, REPORTER_HOME_URL, HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS, SFI_ADDITIONAL_DETAILS_SECTION_NAME } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { DATE_PLACEHOLDER } from '../../../../../src/app/app-constants';
import { compareDates, getDateObjectFromTimeStamp, parseDateWithoutTimestamp } from '../../../../../../fibi/src/app/common/utilities/date-utilities';
import { slideInOut } from '../../../../../../fibi/src/app/common/utilities/animations';
import { NavigationService } from '../../../common/services/navigation.service';
import { isEmptyObject, scrollIntoView } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
    selector: 'app-view-relationship-details',
    templateUrl: './view-relationship-details.component.html',
    styleUrls: ['./view-relationship-details.component.scss'],
    animations: [slideInOut]
})
export class ViewRelationshipDetailsComponent implements OnDestroy {

    @Input() entityId: any;
    @Input() entityNumber: any;
    @Input() isTriggeredFromSlider = false;

    @Output() closeEntityInfoCard: EventEmitter<boolean> = new EventEmitter<boolean>();

    isReadMoreBusinessArea = false;
    isReadMoreUniversity = false;
    isReadMoreRelationWith = false;
    isEnableActivateInactivateSfiModal = false;
    isEditMode = false;
    isCOIAdministrator = true;

    datePlaceHolder = DATE_PLACEHOLDER;
    relationshipsDetails: any = {};
    $subscriptions: Subscription[] = [];
    allRelationQuestionnaires = [];
    updatedRelationshipStatus: string;
    previousUrlBeforeActivate = '';
    mandatoryList = new Map();
    entityVersionList: any = {};
    changedEntityId: any;
    selectedVersionEntityId: any;
    entityDetails: any = {};
    involvementDate = {
        involvementStartDate: null,
        involvementEndDate: null
    }
    additionalDetails: any = {
        sponsorsResearch: false
    };

    constructor(public entityDetailsServices: EntityDetailsService, private _router: Router,
        private _route: ActivatedRoute, public commonService: CommonService, private _navigationService: NavigationService) {
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    async ngOnInit() {
        await this.getEntityDetails(this.entityId);
        this.isEditMode = this.checkForEditMode();
        this.selectedVersionEntityId = this.entityId;
        this.updateRelationshipDetailsStatus();
        this.getSfiVersion();
        this.listenForQuestionnaireSave();
        this.updatePersonEntityRelationships();
        this.isCOIAdministrator = this.commonService.getAvailableRight(['MANAGE_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE']);
    }

    checkForEditMode() {
        if(this._route.snapshot.queryParamMap.get('mode') == 'E' && this.entityDetailsServices.canMangeSfi) {
            return true;
        } else if (this.isTriggeredFromSlider) {
            return false;
        } else if (this.entityDetailsServices.canMangeSfi && !this.relationshipsDetails.isFormCompleted && this.relationshipsDetails.versionStatus != 'INACTIVE') {
            return  true;
        } else {
            return  false;
        }
    }

    getSfiVersion() {
        if (!isEmptyObject(this.relationshipsDetails)) {
            this.$subscriptions.push(this.entityDetailsServices.getSfiVersion(this.relationshipsDetails.personEntityNumber).subscribe((data: any) => {
                this.entityVersionList = data;
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in opening selected version, please try again');
            }));
        }
    }

    updatePersonEntityRelationships() {
        this.$subscriptions.push(this.entityDetailsServices.$addOrDeleteRelation.subscribe((data: any) => {
            this.relationshipsDetails.updateTimestamp = data.updateTimestamp;
            this.relationshipsDetails.isFormCompleted = data.isFormCompleted;
            this.updateEditMode();
            this.updateRelationshipDetailsStatus();
        }));
    }

    versionChange(event) {
        this.entityDetailsServices.isVersionChange = true;
        this.changedEntityId = event.target.value;
        if (!this.isTriggeredFromSlider) {
            this.updateURL({ 'personEntityId': this.getEntityId(), 'personEntityNumber': this.getEntityNumber() });
        }
        this.updateDropDownValue();
        if (this.entityDetailsServices.isRelationshipQuestionnaireChanged || this.entityDetailsServices.isAdditionalDetailsChanged ) {
            this.entityDetailsServices.$emitUnsavedChangesModal.next();
        } else {
            this.loadCurrentVersion();
        }
    }

    updateDropDownValue() {
        const ENTITY_ID = this.selectedVersionEntityId;
        this.selectedVersionEntityId = null;
        setTimeout(() => {
            this.selectedVersionEntityId = ENTITY_ID;
        });
    }

    async loadCurrentVersion() {
        this.entityDetailsServices.currentRelationshipQuestionnaire = {};
        await this.getEntityDetails(this.changedEntityId);
        this.entityId = this.changedEntityId;
        this.selectedVersionEntityId = this.changedEntityId;
        this.updateEditMode();
        this.updateRelationshipDetailsStatus();
    }

    getEntityId() {
        return this._route.snapshot.queryParamMap.get('personEntityId');
    }

    getEntityNumber() {
        return this._route.snapshot.queryParamMap.get('personEntityNumber');
    }

    getEntityDetails(personEntityId, canLoadFirstRelation = true) {
        return new Promise<boolean>((resolve) => {
            this.$subscriptions.push(this.entityDetailsServices.getRelationshipEntityDetails(personEntityId).subscribe((res: any) => {
                this.relationshipsDetails = res.personEntity;
                this.entityDetailsServices.definedRelationships = res.personEntityRelationships;
                this.entityDetailsServices.currentVersionDetails = {
                    versionNumber: this.relationshipsDetails.versionNumber,
                    personEntityNumber: this.relationshipsDetails.personEntityNumber,
                    personEntityId: this.relationshipsDetails.personEntityId
                };
                this.setAdditionalDetails(res.personEntity);
                this.entityDetailsServices.canMangeSfi = this.relationshipsDetails.personId === this.commonService.currentUserDetails.personId ? true : false;
                this.updatedRelationshipStatus = this.relationshipsDetails.versionStatus === 'ACTIVE' ? 'INACTIVE' : 'ACTIVE';
                if(canLoadFirstRelation) {
                    this.triggerOpenQuestionnaire(this.entityDetailsServices.definedRelationships[0]);
                }
                this.getSfiEntityDetails();
                resolve(true);
            }, _error => {
                if (_error.status != 403) {
                    this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                }
                resolve(false);

            }));
        });
    }

    navigateBack() {
        if (this.previousUrlBeforeActivate === '' && (this._navigationService.previousURL.includes('personEntityId') || this._navigationService.previousURL.includes('entity-management') ||
            this._navigationService.previousURL.includes('create-sfi/create') || this._navigationService.previousURL === '')) {
            this._router.navigate(['/coi/user-dashboard/entities']);
        } else {
            this.previousUrlBeforeActivate.includes('coi/create-disclosure/') ? this._router.navigateByUrl(this.previousUrlBeforeActivate) : this._router.navigateByUrl(this._navigationService.previousURL);
        }
    }

    private checkMandatoryFilled(): boolean {
        this.mandatoryList.clear();
        if (!this.involvementDate.involvementStartDate) {
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
        if (this.involvementDate.involvementEndDate) {
            this.endDateValidation();
        }
        return this.mandatoryList.size !== 0 ? false : true;
    }

    endDateValidation(): void {
        this.mandatoryList.delete('endDate');
        if (this.involvementDate.involvementStartDate && this.involvementDate.involvementEndDate &&
            (compareDates(this.involvementDate.involvementStartDate, this.involvementDate.involvementEndDate) === 1)) {
            this.mandatoryList.set('endDate', 'Please provide a valid date.');
        }
    }

    setAdditionalDetails(details) {
        this.involvementDate.involvementStartDate = getDateObjectFromTimeStamp(details.involvementStartDate);
        this.involvementDate.involvementEndDate = getDateObjectFromTimeStamp(details.involvementEndDate);
        this.additionalDetails.sponsorsResearch = details.sponsorsResearch;
        this.additionalDetails.instituteResourceInvolvement = details.instituteResourceInvolvement;
        this.additionalDetails.studentInvolvement = details.studentInvolvement;
        this.additionalDetails.staffInvolvement = details.staffInvolvement;
        this.additionalDetails.personEntityId = this.entityId;
        this.additionalDetails.personEntityNumber = this.entityNumber;
    }

    activateInactivateSfi() {
        if ((this.relationshipsDetails.isFormCompleted && (this.entityDetailsServices.isAdditionalDetailsChanged ||
            this.entityDetailsServices.isRelationshipQuestionnaireChanged))) {
            this.saveRelationship();
        }
        this.isEnableActivateInactivateSfiModal = true;
    }

    saveRelationship() {
        if (this.checkMandatoryFilled() && this.entityDetailsServices.isAdditionalDetailsChanged) {
            this.updatePersonEntityAdditionalDetails();
        }
        this.entityDetailsServices.globalSave$.next();
        if (this.entityDetailsServices.isRelationshipQuestionnaireChanged) {
            let index = this.entityDetailsServices.unSavedSections.findIndex(ele => ele.includes('Relationship Questionnaire'));
            if (index >= 0) {
                this.entityDetailsServices.unSavedSections.splice(index, 1);
            }
        }
    }

    updateURL(event) {
        this.previousUrlBeforeActivate = '';
        if (this._navigationService.previousURL.includes('coi/create-disclosure/')) {
            this.previousUrlBeforeActivate = this._navigationService.previousURL;
        }
        this._router.navigate(['/coi/entity-details/entity'],
            { queryParams: { personEntityId: event.personEntityId, personEntityNumber: event.personEntityNumber } });
    }

    goToHome() {
        const reRouteUrl = this.isCOIAdministrator ? ADMIN_DASHBOARD_URL : REPORTER_HOME_URL;
        this._router.navigate([reRouteUrl]);
    }

    closeActivateInactivateSfiModal(event) {
        if (event) {
            if (this.entityId != event.personEntityId) {
                this.updateModifiedVersion(event);
                this.updatedRelationshipStatus = event.versionStatus === 'ACTIVE' ? 'INACTIVE' : 'ACTIVE';
                this.updateEditMode();
            } else {
               this.updateNewStatus(event);
            }
            this.isEnableActivateInactivateSfiModal = false;
        } else {
            this.isEnableActivateInactivateSfiModal = false;
        }
        this.updateHistoryTab();
    }

    updateHistoryTab() {
        if (this.entityDetailsServices.activeTab === 'HISTORY') {
            this.entityDetailsServices.$updateHistory.next(true);
        }
    }

    updateNewStatus(event) {
        if (event.versionStatus) {
            this.relationshipsDetails.versionStatus = event.versionStatus;
            this.updateURL(event);
        }
        this.relationshipsDetails.updateTimestamp = event.updateTimestamp;
        this.updatedRelationshipStatus = event.versionStatus === 'ACTIVE' ? 'INACTIVE' : 'ACTIVE';
        this.updateEditMode();
        this.updateRelationshipDetailsStatus();
        this.triggerOpenQuestionnaire(!isEmptyObject(this.entityDetailsServices.currentRelationshipQuestionnaire) ? this.entityDetailsServices.currentRelationshipQuestionnaire : this.entityDetailsServices.definedRelationships[0]);
    }

    updateEditMode() {
        this.isEditMode = !this.isTriggeredFromSlider && this.entityDetailsServices.canMangeSfi && !this.relationshipsDetails.isFormCompleted && !['INACTIVE', 'ARCHIVE'].includes(this.relationshipsDetails.versionStatus);
    }

    triggerOpenQuestionnaire(questionnaire) {
        setTimeout(() => {
            this.entityDetailsServices.$openQuestionnaire.next(questionnaire);
        })
    }

    updateRelationshipDetailsStatus() {
        const QUEST_REQ_OBJ_LIST = [];
        this.entityDetailsServices.definedRelationships.forEach(rel => {
            this.setQuestionnaireRequestObject(rel.validPersonEntityRelTypeCode, QUEST_REQ_OBJ_LIST);
        });
        if (QUEST_REQ_OBJ_LIST.length) {
            this.checkQuestionnaireCompleted(QUEST_REQ_OBJ_LIST)
        }
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
            questionnaireMode: this.isEditMode ? 'ACTIVE_ANSWERED_UNANSWERED' : this.relationshipsDetails.isFormCompleted ? 'ANSWERED' : 'ACTIVE_ANSWERED_UNANSWERED'
        }));
    }

    getApplicableQuestionnaire(requestObject) {
        requestObject = JSON.parse(JSON.stringify(requestObject));
        return this.entityDetailsServices.getApplicableQuestionnaire(requestObject);
    }

    checkQuestionnaireCompleted(questionList) {
        this.$subscriptions.push(forkJoin(...questionList).subscribe(data => {
            this.allRelationQuestionnaires = [];
            data.forEach((d: any) => {
                if (d.applicableQuestionnaire.length) {
                    this.entityDetailsServices.relationshipCompletedObject[d.moduleSubItemKey] = d.applicableQuestionnaire.every(questionnaire => questionnaire.QUESTIONNAIRE_COMPLETED_FLAG === 'Y');
                    this.allRelationQuestionnaires = [...this.allRelationQuestionnaires, ...d.applicableQuestionnaire];
                } else {
                    this.entityDetailsServices.relationshipCompletedObject[d.moduleSubItemKey] = true;
                }
            })
            this.triggerFormCompleted();
            this.entityDetailsServices.isRelationshipQuestionnaireChanged = false;
        }, err => {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    triggerFormCompleted() {
        if (!this.isTriggeredFromSlider && this.canCallFormCompleted()) {
            this.$subscriptions.push(this.entityDetailsServices.checkFormCompleted(this.entityId).subscribe((data: any) => {
                this.relationshipsDetails.isFormCompleted = data.isFormCompleted;
                this.updateEditMode();
                this.updateURL(this.relationshipsDetails);
                this.triggerOpenQuestionnaire(!isEmptyObject(this.entityDetailsServices.currentRelationshipQuestionnaire) ? this.entityDetailsServices.currentRelationshipQuestionnaire : this.entityDetailsServices.definedRelationships[0]);
            }));
        }
    }

    canCallFormCompleted() {
        return ((this.isAllQuestionnaireCompleted(this.allRelationQuestionnaires) != this.relationshipsDetails.isFormCompleted)
            || (this.entityDetailsServices.isRelationshipQuestionnaireChanged && this.isAllQuestionnaireCompleted(this.allRelationQuestionnaires)));
    }

    isAllQuestionnaireCompleted(questionnaireList) {
        return questionnaireList.every(questionnaire => questionnaire.QUESTIONNAIRE_COMPLETED_FLAG === 'Y');
    }

    updatePersonEntityAdditionalDetails() {
        this.additionalDetails.involvementStartDate = parseDateWithoutTimestamp(this.involvementDate.involvementStartDate);
        if (this.involvementDate.involvementEndDate) {
            this.additionalDetails.involvementEndDate = parseDateWithoutTimestamp(this.involvementDate.involvementEndDate);
        }
        this.$subscriptions.push(this.entityDetailsServices.updateAdditionalDetails(this.additionalDetails).subscribe((res: any) => {
            this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Significant Financial Interest updated successfully.');
            this.relationshipsDetails.updateTimestamp = res.updateTimestamp;
            this.relationshipsDetails.involvementStartDate = res.involvementStartDate;
            this.relationshipsDetails.involvementEndDate = res.involvementEndDate;
            this.relationshipsDetails.sponsorsResearch = res.sponsorsResearch;
            this.updateEditMode();
            this.entityDetailsServices.isAdditionalDetailsChanged = false;
            let index = this.entityDetailsServices.unSavedSections.findIndex(ele => ele.includes(SFI_ADDITIONAL_DETAILS_SECTION_NAME));
            if (index >= 0) {
                this.entityDetailsServices.unSavedSections.splice(index, 1);
            }
            this.updateURL(this.relationshipsDetails);
        }, error => {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    listenForQuestionnaireSave() {
        this.$subscriptions.push(this.entityDetailsServices.$saveQuestionnaireAction.subscribe((params: any) => {
            if (params) {
                this.updateRelationshipDetailsStatus();
                this.relationshipsDetails.updateTimestamp = params.ANS_UPDATE_TIMESTAMP;
                this.commonService.showToast(HTTP_SUCCESS_STATUS, `Relationship saved successfully `);
            } else {
                this.commonService.showToast(HTTP_ERROR_STATUS, `Error in saving relationship. Please try again.`);
            }
        }));
    }

    viewEntityDetails() {
        this.closeEntityInfoCard.emit(false);
        this._router.navigate(['/coi/entity-management/entity-details'], { queryParams: { entityManageId: this.entityDetails.entityId } });
    }

    modifySfi() {
        this.$subscriptions.push(this.entityDetailsServices.modifyPersonEntity({ personEntityId: this.entityId, personEntityNumber: this.entityNumber }).subscribe((res: any) => {
            this.updateModifiedVersion(res, false);
        }));
    }

    async updateModifiedVersion(res, canLoadFirstRelation = true) {
        this.entityId = new Number(res.personEntityId);
        this.isEditMode = true;
        this.selectedVersionEntityId = this.entityId;
        await this.getEntityDetails(this.entityId, canLoadFirstRelation);
        this.getSfiVersion();
        this.updateRelationshipDetailsStatus();
        if(!canLoadFirstRelation) {
            this.triggerOpenQuestionnaire(isEmptyObject(this.entityDetailsServices.currentRelationshipQuestionnaire) ? this.entityDetailsServices.definedRelationships[0] : this.entityDetailsServices.currentRelationshipQuestionnaire);
        }
        this.updateURL({ 'personEntityId': this.entityId, 'personEntityNumber': this.relationshipsDetails.personEntityNumber });
    }

    addUnSavedChanges() {
        this.entityDetailsServices.isAdditionalDetailsChanged = true;
        if (!this.entityDetailsServices.unSavedSections.some(ele => ele.includes(SFI_ADDITIONAL_DETAILS_SECTION_NAME))) {
            this.entityDetailsServices.unSavedSections.push(SFI_ADDITIONAL_DETAILS_SECTION_NAME);
        }
    }

    openRelationshipQuestionnaire(coiFinancialEntityDetail) {
        if (this.entityDetailsServices.isAdditionalDetailsChanged) {
            this.entityDetailsServices.$emitUnsavedChangesModal.next();
            this.entityDetailsServices.toBeActiveTab = 'QUESTIONNAIRE';
        } else {
                this.entityDetailsServices.currentRelationshipQuestionnaire = coiFinancialEntityDetail;
                this.entityDetailsServices.activeTab = 'QUESTIONNAIRE';
                this.triggerOpenQuestionnaire(coiFinancialEntityDetail);
        }
    }

    scrollPosition(event) {
        if (event) {
            scrollIntoView('focusQuestionnaire');
        }
    }

    async openRelationShipSection() {
        this.entityDetailsServices.toBeActiveTab = 'RELATION_DETAILS';
        if (this.entityDetailsServices.isRelationshipQuestionnaireChanged) {
            this.entityDetailsServices.$emitUnsavedChangesModal.next({ details: null, isLeaveFromRelationTab: true });
        } else {
            this.entityDetailsServices.activeTab = 'RELATION_DETAILS';
            await this.getEntityDetails(this.entityId);
            this.mandatoryList.clear();
        }
    }

    openHistorySection() {
        this.entityDetailsServices.toBeActiveTab = 'HISTORY';
        if (this.entityDetailsServices.isRelationshipQuestionnaireChanged || this.entityDetailsServices.isAdditionalDetailsChanged) {
            this.entityDetailsServices.$emitUnsavedChangesModal.next({ details: null, isLeaveFromRelationTab: true });
        } else {
            this.entityDetailsServices.activeTab = 'HISTORY';
        }
    }

    saveOrAddRelationshipModal() {
        this.entityDetailsServices.$triggerAddRelationModal.next(true);
    }

    canAddRelationship() {
        return (this.isEditMode && this.entityDetailsServices.canMangeSfi && this.entityDetailsServices.remainingRelationships.length);
    }

    isRelationshipDetailsFetched() {
        return !isEmptyObject(this.relationshipsDetails);
    }

    canShowMoreActions() {
        return this.relationshipsDetails.versionStatus != 'ARCHIVE' && this.entityDetailsServices.canMangeSfi;
    }

    showViewButton() {
        return  this.commonService.getAvailableRight(['MANAGE_ENTITY', 'VIEW_ENTITY']) && !['entity-management/entity-details'].some(ele => this._router.url.includes(ele))
    }

    private getSfiEntityDetails(): void {
        this.$subscriptions.push(this.entityDetailsServices.getCoiEntityDetails(this.entityId).subscribe((res: any) => {
            this.entityDetails = res.coiEntity;
        }, _error => {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

}
