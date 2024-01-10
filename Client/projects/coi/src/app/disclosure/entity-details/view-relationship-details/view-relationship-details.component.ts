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
import { deepCloneObject, hideModal, isEmptyObject, scrollIntoView } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';

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
    @Input() entityNumber: any;
    isReadMoreBusinessArea = false;
    isReadMoreUniversity = false;
    isReadMoreRelationWith = false;
    datePlaceHolder = DATE_PLACEHOLDER;
    isEditMode = false;
    @Input() isShowHeader: boolean;
    // isQuestionnaireCompleted = false;
    @Input() isTriggeredFromSlider = false;
    isEnableActivateInactivateSfiModal = false;
    allRelationQuestionnaires = [];
    @Input() entityDetails: any = {};
    isHoverCardViewMore = false;
    hasUserExpanded = false;
    isFinalizeApi = false;
    updatedRelationshipStatus: string;
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
    deleteRelationshipDetails: any;
    entityVersionList: any = {};

    constructor( public entityDetailsServices: EntityDetailsService, private _router: Router,
                 private _route: ActivatedRoute, public commonService: CommonService, private _navigationService: NavigationService ) {
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    async ngOnInit() {
        this.getQueryParamChange();
        await this.getEntityDetails(this.getEntityId());
        this.isEditMode = this._route.snapshot.queryParamMap.get('mode') == 'E' ? true :  this.isTriggeredFromSlider ? false : this.relationshipsDetails.isFormCompleted ? false : true;
        this.getQuestionnaire();
        this.getSfiVersion();
        this.subscribeScrollAction();
        this.subscribeSliderScrollAction();
        this.updateFormComplete();
        this.isCOIAdministrator = this.commonService.getAvailableRight(['MANAGE_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE']);
    }

    getSfiVersion() {
        if(!isEmptyObject(this.relationshipsDetails)) {
            this.$subscriptions.push(this.entityDetailsServices.getSfiVersion(this.relationshipsDetails.personEntityNumber).subscribe((data: any) => {
                this.entityVersionList = data;
            }, err => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Error in opening selected version, please try again');
            }));
        }
      }

      async versionChange() {
        await this.getEntityDetails(this.entityId);
        await this.getDefinedRelationships(this.entityId);
        this.entityDetailsServices.$openQuestionnaire.next(this.entityDetailsServices.definedRelationships[0]);
        const CURRENT_VERSION = this.entityVersionList.find(e => e.personEntityId == this.entityId);
        this.isEditMode = CURRENT_VERSION.versionStatus == 'ACTIVE' && !this.isTriggeredFromSlider ? true : false;
        this.getQuestionnaire();
      }

    async ngOnChanges() {
        if (this.updateRelationshipDetails && this.updateRelationshipDetails.length) {
            this.relationshipsDetails.updateTimestamp = this.updateRelationshipDetails[0].updateTimestamp;
            this.updateRelationshipDetails.forEach(element => {
                this.personEntityRelationships.push(element);
            });
            this.updateRelationshipDetails = [];
            this.getQuestionnaire();
        }
        this.listenForQuestionnaireSave();
    }

    updateFormComplete() {
        this.entityDetailsServices.$updateFormCompleted.subscribe((data: any) => {
            if (data) {
                this.relationshipsDetails.isFormCompleted = data.isFormCompleted;
                this.isEditMode = data.isFormCompleted ? false : true;
            }
        });
    }

    getEntityId() {
        return this._route.snapshot.queryParamMap.get('personEntityId') || this.entityId;
    }

    getEntityNumber() {
        return this._route.snapshot.queryParamMap.get('personEntityNumber') || null;
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
                this.updatedRelationshipStatus = this.relationshipsDetails.versionStatus === 'ACTIVE' ?  'INACTIVE' : 'ACTIVE';
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
        if ((this.relationshipsDetails.isFormCompleted && (this.isChangesInFieldValue||
            this.entityDetailsServices.isRelationshipQuestionnaireChanged))) {
            this.saveRelationship();
        }
        this.isEnableActivateInactivateSfiModal = true;
    }

    saveRelationship() {
        if (this.checkMandatoryFilled() && this.isChangesInFieldValue) {
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
        if (this.relationshipsDetails.isFormCompleted) {
            if(this._navigationService.previousURL.includes('coi/create-disclosure/')) {
                this.previousUrlBeforeActivate = this._navigationService.previousURL;
            }
            this._router.navigate(['/coi/entity-details/entity'],
                { queryParams: { personEntityId: event.personEntityId, personEntityNumber: event.personEntityNumber } });
        }
    }

    goToHome() {
        const reRouteUrl = this.isCOIAdministrator ? ADMIN_DASHBOARD_URL : REPORTER_HOME_URL;
        this._router.navigate([reRouteUrl]);
    }

    closeActivateInactivateSfiModal(event) {
        if (event) {
            this.entityId = new Number(event.personEntityId);
            this.relationshipsDetails.isRelationshipActive = event.isRelationshipActive;
            if (event.versionStatus) {
                this.relationshipsDetails.versionStatus = event.versionStatus;
                this.updateURL(event);
            }
            this.relationshipsDetails.updateTimestamp = event.updateTimestamp;
            this.updatedRelationshipStatus = event.versionStatus === 'ACTIVE' ?  'INACTIVE' : 'ACTIVE';
            this.isEditMode = event.versionStatus === 'INACTIVE' ? false : this.isEditMode;
            this.entityDetailsServices.$openQuestionnaire.next(!isEmptyObject(this.entityDetailsServices.currentRelationshipQuestionnaire) ? this.entityDetailsServices.currentRelationshipQuestionnaire : this.entityDetailsServices.definedRelationships[0]);
            this.isEnableActivateInactivateSfiModal = false;
            if (this.isFinalizeApi) {
                this.isFinalizeApi = false;
            }
            if (this.entityId != event.personEntityId) {
                if(this._navigationService.previousURL.includes('coi/create-disclosure/')) {
                    this.previousUrlBeforeActivate = this._navigationService.previousURL;
                }
                this._router.navigate(['/coi/entity-details/entity'],
                    { queryParams: { personEntityId: event.personEntityId, personEntityNumber: event.personEntityNumber } });
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
        if(QUEST_REQ_OBJ_LIST.length) {
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
                    this.entityDetailsServices.relationshipCompletedObject[d.moduleSubItemKey] = d.applicableQuestionnaire.every(questionnaire => questionnaire.QUESTIONNAIRE_COMPLETED_FLAG === 'Y');
                    this.combineQuestionnaireList(d.applicableQuestionnaire);
                } else {
                    this.entityDetailsServices.relationshipCompletedObject[d.moduleSubItemKey] = true;
                }
            })
            if(this.isAllQuestionnaireCompleted(this.allRelationQuestionnaires) && this.entityDetailsServices.isRelationshipQuestionnaireChanged) {
                this.$subscriptions.push(this.entityDetailsServices.checkFormCompleted(this.getEntityId()).subscribe((data: any) => {
                    this.relationshipsDetails.isFormCompleted = data.isFormCompleted;
                    this.isEditMode = !this.relationshipsDetails.isFormCompleted;
                    this.updateURL(this.relationshipsDetails);
                    this.entityDetailsServices.$openQuestionnaire.next(!isEmptyObject(this.entityDetailsServices.currentRelationshipQuestionnaire) ? this.entityDetailsServices.currentRelationshipQuestionnaire : this.entityDetailsServices.definedRelationships[0]);
                }));
            }   
            this.entityDetailsServices.isRelationshipQuestionnaireChanged = false;         
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
            this.isEditMode = !this.relationshipsDetails.isFormCompleted;
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
                this.getQuestionnaire();
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
        this.$subscriptions.push(this.entityDetailsServices.modifyPersonEntity({ personEntityId: this.getEntityId(), personEntityNumber: this.getEntityNumber() }).subscribe((res: any) => {
            this.entityId = new Number(res.personEntityId);
            this.isEditMode = true;
            this.entityDetailsServices.$openQuestionnaire.next(!isEmptyObject(this.entityDetailsServices.currentRelationshipQuestionnaire) ? this.entityDetailsServices.currentRelationshipQuestionnaire : this.entityDetailsServices.definedRelationships[0]);
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

    getDefinedRelationships(personEntityId) {
        const REQ_BODY = {
            'personEntityId': personEntityId
        };
        return new Promise<boolean>((resolve) => {
            this.$subscriptions.push(this.entityDetailsServices.getPersonEntityRelationship(REQ_BODY).subscribe((res: any) => {
                if (res.length) {
                    this.entityDetailsServices.definedRelationships = res || [];
                }
                resolve(true);
            }, error => {
                this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                resolve(false);
            }));
        });
    }

    emittedDeletedRelationship() {
        if (this.deleteRelationshipDetails) {
            let delIndex = this.personEntityRelationships.findIndex(ele => ele.personEntityRelId == this.deleteRelationshipDetails.personEntityRelId);
            if (delIndex >= 0) {
                this.personEntityRelationships.splice(delIndex, 1);
                this.entityDetailsServices.definedRelationships.splice(delIndex, 1);
                this.addToAvailableRelation(this.deleteRelationshipDetails.validPersonEntityRelType);
                if (this.entityDetailsServices.definedRelationships.length) {
                    this.entityDetailsServices.definedRelationships.length > delIndex ?
                        this.openRelationshipQuestionnaire(this.entityDetailsServices.definedRelationships[delIndex]) :
                        this.openRelationshipQuestionnaire(this.entityDetailsServices.definedRelationships[this.entityDetailsServices.definedRelationships.length - 1]);
                }
                if (!this.entityDetailsServices.definedRelationships.length) {
                    this.isEditMode = true;
                }
            }
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

    deleteRelationship() {
        let VALID_REL_TYPE_CODE = this.deleteRelationshipDetails.validPersonEntityRelTypeCode;
        this.$subscriptions.push(this.entityDetailsServices.deletePersonEntityRelationship
            (this.deleteRelationshipDetails.personEntityRelId, this.deleteRelationshipDetails.personEntityId).subscribe(async (data) => {
                this.entityDetailsServices.$updateFormCompleted.next(data);
                if (VALID_REL_TYPE_CODE in this.entityDetailsServices.relationshipCompletedObject) {
                    delete this.entityDetailsServices.relationshipCompletedObject[VALID_REL_TYPE_CODE];
                }
                this.emittedDeletedRelationship();
                this.commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationship deleted successfully.');
                this.updateURL(this.relationshipsDetails);
                this.deleteRelationshipDetails = null;
                hideModal('deleteRelationModal');
            }, _err => {
                if (_err.status === 405) {
                    hideModal('deleteRelationModal');
                    this.entityDetailsServices.concurrentUpdateAction = 'Delete Relationship'
                } else {
                    this.commonService.showToast(HTTP_ERROR_STATUS, `Error in deleting relationship.`);
                }
            }));
    }

    canAddRelationship() {
       return (this.isEditMode && this.entityDetailsServices.canMangeSfi && this.entityDetailsServices.availableRelationships?.length);
    }
    
    
    private async addToAvailableRelation(relation: any) {
       let availableRelationships = await this.getRelationshipLookUp();
       let relationIndex = availableRelationships.findIndex(ele => ele.validPersonEntityRelTypeCode == relation.validPersonEntityRelTypeCode);
       this.entityDetailsServices.groupedRelations = {};
       if(this.entityDetailsServices.availableRelationships.length && this.entityDetailsServices.availableRelationships[relationIndex] && this.entityDetailsServices.availableRelationships[relationIndex].validPersonEntityRelTypeCode == relation.validPersonEntityRelTypeCode) {
            this.entityDetailsServices.availableRelationships.splice(relationIndex, 1);
       }
        this.entityDetailsServices.availableRelationships.splice(relationIndex, 0 ,relation);
        if (this.entityDetailsServices.availableRelationships.length) {
            this.entityDetailsServices.groupedRelations = this.groupBy(deepCloneObject(this.entityDetailsServices.availableRelationships), "coiDisclosureType", "description");
        }
    }

    async getRelationshipLookUp(): Promise<any> {
        try {
            const response = await this.entityDetailsServices.addSFILookUp();
            return response;
        } catch (error) {
            this.commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }
    }

    groupBy(jsonData, key, innerKey) {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[key][innerKey]] = relationsTypeGroup[item[key][innerKey]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

    isRelationshipDetailsFetched() {
        return !isEmptyObject(this.relationshipsDetails);
    }

    openQuestionnaireTab() {
        this.entityDetailsServices.selectedTab = 'QUESTIONNAIRE';
        setTimeout(() => {
            this.entityDetailsServices.$openQuestionnaire.next(this.entityDetailsServices.definedRelationships[0]);
        });    
    }

}
