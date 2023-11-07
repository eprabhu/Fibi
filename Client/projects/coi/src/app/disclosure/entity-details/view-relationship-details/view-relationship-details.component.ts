import { Component, EventEmitter, HostListener, Input, OnChanges, OnDestroy, Output, SimpleChanges } from '@angular/core';
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
import { scrollIntoView } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';

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
    datePlaceHolder = DATE_PLACEHOLDER;
    @Input() isEditMode = false;
    @Input() isShowHeader: boolean;
    isQuestionnaireCompleted = false;
    @Input() isTriggeredFromSlider = false;
    isEnableActivateInactivateSfiModal = false;
    allRelationQuestionnaires = [];
    @Input() entityDetails: any = {};
    isHoverCardViewMore = false;
    hasUserExpanded = false;
    isFinalizeApi = false;
    isRelationshipActive = false;
    @Output() closeEntityInfoCard: EventEmitter<boolean> = new EventEmitter<boolean>();
    previousUrlBeforeActivate = '';
    isCOIAdministrator = true;
    isChangesInFieldValue = false;
    involvementDate =  {
      involvementStartDate: null,
      involvementEndDate: null
    }
    additionalDetails: any = {
      sponsorsResearch: false
    };
    mandatoryList = new Map();
    deleteRelationshipEvent: any;

    constructor( public entityDetailsServices: EntityDetailsService, private _router: Router,
                 private _route: ActivatedRoute, public commonService: CommonService, private _navigationService: NavigationService ) {
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    async ngOnInit() {
        this.getQueryParamChange();
        await this.getEntityDetails(this.getEntityId());
        this.getQuestionnaire();
        this.subscribeScrollAction();
        this.subscribeSliderScrollAction();
        this.isCOIAdministrator = this.commonService.getAvailableRight(['MANAGE_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE']);
    }

    async ngOnChanges() {
        if (!this.isEditMode) {
            this.isQuestionnaireCompleted = true;
        }
        if (this.updateRelationshipDetails && this.updateRelationshipDetails.length) {
            this.relationshipsDetails.updateTimestamp = this.updateRelationshipDetails[0].updateTimestamp;
            this.updateRelationshipDetails.forEach(element => {
                this.personEntityRelationships.push(element);
            });
            this.updateRelationshipDetails = [];
        }
        this.listenForQuestionnaireSave();
        if (this.isEditMode) {
            this.getQuestionnaire();
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
                this.entityDetailsServices.currentVersionDetails =  {
                    versionNumber: this.relationshipsDetails.versionNumber, 
                    personEntityNumber: this.relationshipsDetails.personEntityNumber, 
                    personEntityId: this.relationshipsDetails.personEntityId
                };
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
        if (this.previousUrlBeforeActivate === '' && (this._navigationService.previousURL.includes('personEntityId') || this._navigationService.previousURL.includes('entity-management') ||
            this._navigationService.previousURL.includes('create-sfi/create') || this._navigationService.previousURL === '')) {
            this._router.navigate(['/coi/user-dashboard/entities']);
        } else {
            this.previousUrlBeforeActivate.includes('coi/create-disclosure/') ? this._router.navigateByUrl(this.previousUrlBeforeActivate) : this._router.navigateByUrl(this._navigationService.previousURL);
        }
    }

    /**
     *
     * @returns need to check this
     */

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
            let index = this.entityDetailsServices.unSavedSections.findIndex(ele => ele.includes('Relationship Questionnaire'));
            if (index >= 0) {
                this.entityDetailsServices.unSavedSections.splice(index, 1);
            }
        }
    }

    goToHome() {
        const reRouteUrl = this.isCOIAdministrator ? ADMIN_DASHBOARD_URL : REPORTER_HOME_URL;
        this._router.navigate([reRouteUrl]);
    }

    closeActivateInactivateSfiModal(event) {
        if (event) {
            this.entityId = new Number(event.personEntityId);
            this.previousUrlBeforeActivate = '';
            this.relationshipsDetails.isRelationshipActive = event.isRelationshipActive;
            if (event.versionStatus) {
                this.relationshipsDetails.versionStatus = event.versionStatus;
                if (this.isQuestionnaireCompleted) {
                    if(this._navigationService.previousURL.includes('coi/create-disclosure/')) {
                        this.previousUrlBeforeActivate = this._navigationService.previousURL;
                    }
                    this._router.navigate(['/coi/entity-details/entity'],
                        { queryParams: { personEntityId: event.personEntityId, mode: 'view' } });
                        this.getEntityDetails(this.getEntityId());
                }
            }
            this.relationshipsDetails.updateTimestamp = event.updateTimestamp;
            this.sfiStatus = this.getSfiStatus();
            this.isRelationshipActive = this.sfiStatus === 'Draft' || this.sfiStatus === 'Inactive' ? false : true;
            this.isEnableActivateInactivateSfiModal = false;
            if (this.isFinalizeApi) {
                this.isFinalizeApi = false;
            }
            if (this.entityId != event.personEntityId) {
                if(this._navigationService.previousURL.includes('coi/create-disclosure/')) {
                    this.previousUrlBeforeActivate = this._navigationService.previousURL;
                }
                this._router.navigate(['/coi/entity-details/entity'],
                    { queryParams: { personEntityId: event.personEntityId, mode: 'view' } });
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
        QUEST_REQ_OBJ_LIST.length ? this.checkQuestionnaireCompleted(QUEST_REQ_OBJ_LIST) : this.isQuestionnaireCompleted = false;
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
            data.forEach((d: any) =>{
                if(d.applicableQuestionnaire.length) {
                    this.entityDetailsServices.relationshipCompletedObject[d.applicableQuestionnaire[0].MODULE_SUB_ITEM_KEY] = d.applicableQuestionnaire.every(questionnaire => questionnaire.QUESTIONNAIRE_COMPLETED_FLAG === 'Y');
                    this.combineQuestionnaireList(d.applicableQuestionnaire);
                }
            })
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
        this.additionalDetails.involvementStartDate = parseDateWithoutTimestamp(this.involvementDate.involvementStartDate);
        if (this.involvementDate.involvementEndDate) {
            this.additionalDetails.involvementEndDate = parseDateWithoutTimestamp(this.involvementDate.involvementEndDate);
        }
        this.$subscriptions.push(this.entityDetailsServices.updateAdditionalDetails(this.additionalDetails).subscribe((res: any) => {
            this.isChangesInFieldValue = false;
            this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Significant Financial Interest updated successfully.');
            this.relationshipsDetails.updateTimestamp = res.updateTimestamp;
            this.relationshipsDetails.involvementStartDate = res.involvementStartDate;
            this.relationshipsDetails.involvementEndDate = res.involvementEndDate;
            this.relationshipsDetails.sponsorsResearch = res.sponsorsResearch;
            this.entityDetailsServices.isAdditionalDetailsChanged = false;
            let index = this.entityDetailsServices.unSavedSections.findIndex(ele => ele.includes(SFI_ADDITIONAL_DETAILS_SECTION_NAME));
            if (index >= 0) {
                this.entityDetailsServices.unSavedSections.splice(index, 1);
            }
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
        this.closeEntityInfoCard.emit(false);
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
            this.entityId = new Number(res.personEntityId);
            this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: res.personEntityId, mode: 'edit' } });
        }));
    }

    getQueryParamChange() {
        this.$subscriptions.push(this._route.queryParams.subscribe(params => {
            this.getEntityDetails(this.getEntityId());
        }));
    }

    addUnSavedChanges() {
        this.entityDetailsServices.isAdditionalDetailsChanged = true;
        this.isChangesInFieldValue = true;
        if(!this.entityDetailsServices.unSavedSections.some(ele => ele.includes(SFI_ADDITIONAL_DETAILS_SECTION_NAME))) {
            this.entityDetailsServices.unSavedSections.push(SFI_ADDITIONAL_DETAILS_SECTION_NAME);
        }
    }

    openRelationDetails() {
        this.$subscriptions.push(this.entityDetailsServices.getCurrentId(this.relationshipsDetails.personEntityNumber).subscribe((data: any) => {
            this._router.navigate(['/coi/entity-details/entity'],
                { queryParams: { personEntityId: data, mode: 'view' } });
        }, err => {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in opening current version, please try again');
        }));
    }

    openRelationshipQuestionnaire(coiFinancialEntityDetail) {
        this.entityDetailsServices.currentRelationshipQuestionnaire = coiFinancialEntityDetail;
        this.entityDetailsServices.clickedTab = 'QUESTIONNAIRE';
        if (this.entityDetailsServices.isAdditionalDetailsChanged) {
            this.entityDetailsServices.$emitUnsavedChangesModal.next();
        } else {
            this.entityDetailsServices.selectedTab = 'QUESTIONNAIRE';
            setTimeout(() => {
                this.entityDetailsServices.$openQuestionnaire.next(coiFinancialEntityDetail);
            });
        }
    }

    updateRelationship(event) {
        this.updateRelationshipDetails = event;
    }

    scrollPosition(event) {
        if (event) {
            scrollIntoView('focusQuestionnaire');
        }
    }

    async openRelationShipSection() {
        this.entityDetailsServices.clickedTab = 'RELATION_DETAILS';
        if (this.entityDetailsServices.isRelationshipQuestionnaireChanged) {
            this.entityDetailsServices.$emitUnsavedChangesModal.next({ details: null, isLeaveFromRelationTab: true });
        } else {
            this.entityDetailsServices.selectedTab = 'RELATIONSHIP_DETAILS';
            await this.getEntityDetails(this.getEntityId());
            this.mandatoryList.clear();
        }
    }

    openHistorySection() {
        this.entityDetailsServices.clickedTab = 'HISTORY';
        if (this.entityDetailsServices.isRelationshipQuestionnaireChanged || this.entityDetailsServices.isAdditionalDetailsChanged) {
            this.entityDetailsServices.$emitUnsavedChangesModal.next({ details: null, isLeaveFromRelationTab: true });
        } else {
            this.entityDetailsServices.selectedTab = 'HISTORY';
        }
    }

    emittedDeletedRelationship(event) {
        this.deleteRelationshipEvent = event;
        if (this.deleteRelationshipEvent && this.deleteRelationshipEvent.isDeleted) {
            this.relationshipsDetails.updateTimestamp = this.deleteRelationshipEvent.updatedTimestamp;
            let delIndex = this.personEntityRelationships.findIndex(ele => ele.personEntityRelId == this.deleteRelationshipEvent.removeRelId);
            if (delIndex >= 0) {
                this.personEntityRelationships.splice(delIndex, 1);
            }
            this.deleteRelationshipEvent.isDeleted = false;
        }
        if (this.isEditMode) {
            this.getQuestionnaire();
        }
    }

    saveOrAddRelationshipModal() {
        this.mandatoryList.clear();
        if (!this.entityDetailsServices.isAdditionalDetailsChanged) {
            setTimeout(() => {
                this.entityDetailsServices.$triggerAddRelationModal.next(true);
            });
        } else {
            if (this.checkMandatoryFilled()) {
                this.saveRelationship();
                setTimeout(() => {
                    this.entityDetailsServices.$triggerAddRelationModal.next(true);
                });
            } else {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Please fill mandatory fields to proceed with Add Relation');
            }
        }
    }

    isSaveAndActive() {
        return this.isEditMode && (this.entityDetailsServices.canMangeSfi && this.isQuestionnaireCompleted &&  
            (this.sfiStatus ==='Inactive' || this.sfiStatus == 'Draft'));
    }

    subscribeScrollAction() {
        this.commonService.$ScrollAction.subscribe((data: any) => {
           if(data.pageYOffset > 0) {
               this.setStickyNavigation();
            } else {
                this.resetStickyNavigation();
            }
        })
    }

    subscribeSliderScrollAction() {
        if(this.isTriggeredFromSlider) {
            this.commonService.$sliderScrollAction.subscribe((data: any) => {
                if(data.pageYOffset > 0) {
                    this.setStickyNavigation();
                } else {
                     this.resetStickyNavigation();
                 }
             })
        }
    }

    setStickyNavigation() {
        let SFI_TOP_CARD = document.getElementById('sfi-top-card');
        let SFI_NAV_BAR = document.getElementById('sfi-nav-bar');
        if (SFI_NAV_BAR && SFI_TOP_CARD) {
            let SFI_TOP_CARD_HEIGHT =  SFI_TOP_CARD.offsetHeight;
            SFI_NAV_BAR.style.top = (this.isTriggeredFromSlider ? (SFI_TOP_CARD_HEIGHT - 1) : SFI_TOP_CARD_HEIGHT) + 'px';
            SFI_NAV_BAR.style.paddingTop = '0px';
        }
    }

    resetStickyNavigation() {
        let SFI_NAV_BAR = document.getElementById('sfi-nav-bar');
        if (SFI_NAV_BAR) {
            SFI_NAV_BAR.style.paddingTop = '12.5px';
            SFI_NAV_BAR.style.top = '0px';
        }
    }

}
