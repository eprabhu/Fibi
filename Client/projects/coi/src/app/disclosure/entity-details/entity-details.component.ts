import { Component, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityDetailsService } from './entity-details.service';
import { deepCloneObject, hideModal, isEmptyObject, openModal, scrollIntoView } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { Subscription } from 'rxjs';
import { NavigationService } from '../../common/services/navigation.service';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { CommonService } from '../../common/services/common.service';
import { HTTP_ERROR_STATUS, SFI_ADDITIONAL_DETAILS_SECTION_NAME } from '../../app-constants';
import { ViewRelationshipDetailsComponent } from './view-relationship-details/view-relationship-details.component';

@Component({
    selector: 'app-entity-details',
    templateUrl: './entity-details.component.html',
    styleUrls: ['./entity-details.component.scss']
})

export class EntityDetailsComponent implements OnInit, OnDestroy {
    @ViewChild(ViewRelationshipDetailsComponent) viewRelationComponent!: ViewRelationshipDetailsComponent;
    @Input() entityId: any;
    @Input() entityNumber: any;
    isTriggeredFromSlider = false;
    $subscriptions: Subscription[] = [];
    @Output() closeAction: EventEmitter<boolean> = new EventEmitter<boolean>();
    questionnaireSection: any = '';
    definedRelationships = [];
    selectedQuestionnaire: any;
    relationValidationMap = new Map();
    isSaving = false;

    constructor(public entityDetailService: EntityDetailsService, private _route: ActivatedRoute, private _router: Router,
        private _commonService: CommonService, private _navigationService: NavigationService) {
        this.clearSfiNavBarStyle();
    }

    entityDetails = {};

    async ngOnInit() {
        this.entityDetailService.selectedTab = 'QUESTIONNAIRE';
        this.isTriggeredFromSlider = this.checkForUrl();
        this.getQueryParams();
        this.resetServiceValues();
        await this.getDefinedRelationships();
        this.getAvailableRelationship();
        this.openAddRelationModal();
        this.showQuestionnaireLeaveConfirmationModal();
    }

    async getAvailableRelationship() {
        this.entityDetailService.availableRelationships = await this.getRelationshipLookUp();
        this.removeExistingRelation();
    }

    getDefinedRelationships() {
        const REQ_BODY = {
            'personEntityId': this._route.snapshot.queryParamMap.get('personEntityId') || this.entityId
        };
        return new Promise<boolean>((resolve) => {
            this.$subscriptions.push(this.entityDetailService.getPersonEntityRelationship(REQ_BODY).subscribe((res: any) => {
                if (res.length) {
                    this.entityDetailService.definedRelationships = res || [];
                    this.openQuestionnaire(this.entityDetailService.definedRelationships[0]);
                } else {
                    this.entityDetailService.selectedTab = 'RELATIONSHIP_DETAILS';
                }
                resolve(true);
            }, error => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                resolve(false);
            }));
        });
    }

    getQueryParams() {
        this.$subscriptions.push(this._route.queryParams.subscribe(params => { 
            this.entityId = params['personEntityId'] || this.entityId;
            this.entityNumber = params['personEntityNumber'] || null;
            this.getSfiEntityDetails();
        }));
    }

    checkForUrl() {
        return ['create-disclosure', 'user-dashboard/entities', 'disclosure/summary', 'entity-management/entity-details', 'user-dashboard/disclosures', 'coi/admin-dashboard'].some(ele => this._router.url.includes(ele))
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

    clearSfiNavBarStyle() {
        document.getElementById('COI_SCROLL').style.removeProperty('overflow');
    }

    fullPageNavigationLeavePage() {
        this.entityDetailService.isRelationshipQuestionnaireChanged = false;
        this.entityDetailService.isAdditionalDetailsChanged = false;
        this.entityDetailService.unSavedSections = [];
        this._router.navigateByUrl(this._navigationService.navigationGuardUrl);
        this.closeUnsavedChangesModal();
    }

    closeUnsavedChangesModal() {
        hideModal('hiddenUnsavedChanges');
    }

    getSfiEntityDetails() {
        this.$subscriptions.push(this.entityDetailService.getCoiEntityDetails(this.entityId).subscribe((res: any) => {
            this.entityDetails = res.coiEntity;
        }, _error => {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }));
    }

    showQuestionnaireLeaveConfirmationModal() {
        this.$subscriptions.push(this.entityDetailService.$emitUnsavedChangesModal.subscribe((data: any) => { //change subscription name
            if (this.entityDetailService.isRelationshipQuestionnaireChanged) {
                this.questionnaireSection = this.entityDetailService.unSavedSections.find(ele => ele.includes('Relationship Questionnaire'));
                openModal('questionnaireUnsavedChanges');
            }
            if (this.entityDetailService.isAdditionalDetailsChanged) {
                openModal('relationDetailsUnSavedChanges');
            }
        }));
    }

    questionnaireChangeModalLeaveTab() {
        this.entityDetailService.isRelationshipQuestionnaireChanged = false;
        let index = this.entityDetailService.unSavedSections.findIndex(ele => ele.includes('Relationship Questionnaire'));
        if (index >= 0) {
            this.entityDetailService.unSavedSections.splice(index, 1);
        }
        if (this.entityDetailService.clickedTab === 'RELATION_DETAILS') {
            this.entityDetailService.selectedTab = 'RELATIONSHIP_DETAILS';
        } else if (this.entityDetailService.clickedTab === 'HISTORY') {
            this.entityDetailService.selectedTab = 'HISTORY';
        } else {
            this.viewRelationComponent.loadCurrentVersion();
            if (this.entityDetailService.currentRelationshipQuestionnaire && !isEmptyObject(this.entityDetailService.currentRelationshipQuestionnaire)) {
                this.openQuestionnaire(this.entityDetailService.currentRelationshipQuestionnaire);
            }
            this.entityDetailService.clickedTab === 'QUESTIONNAIRE';
        }
        hideModal('questionnaireUnsavedChanges');
    }

    openQuestionnaire(entityDetails) {
        this.entityDetailService.selectedTab = 'QUESTIONNAIRE';
        setTimeout(() => {
            this.entityDetailService.$openQuestionnaire.next(entityDetails);
        },500);
    }

    closeSlider(event) {
        this.closeAction.emit(false);
    }

    cancelConcurrency() {
        this.entityDetailService.concurrentUpdateAction = '';
    }

    openAddRelationModal() {
        this.$subscriptions.push(this.entityDetailService.$triggerAddRelationModal.subscribe(async (data: any) => {
            this.removeExistingRelation();
            if (this.entityDetailService.isRelationshipQuestionnaireChanged) {
                this.entityDetailService.globalSave$.next();
            }
            openModal('addRelationshipModal');
        }))
    }

    private removeExistingRelation() {
        this.entityDetailService.groupedRelations = {};
        if (this.entityDetailService.definedRelationships.length) {
            this.entityDetailService.definedRelationships.forEach(element => {
                this.findRelation(element.validPersonEntityRelType.validPersonEntityRelTypeCode);
            });
        } else {
            if (this.entityDetailService.availableRelationships.length) {
                this.entityDetailService.groupedRelations = this.groupBy(deepCloneObject(this.entityDetailService.availableRelationships), "coiDisclosureType", "description");
            }
        }
    }

    private findRelation(financialEntityRelTypeCode: string) {
        this.entityDetailService.groupedRelations = {};
        const RELATION_INDEX = this.entityDetailService.availableRelationships.findIndex(element =>
            element.validPersonEntityRelTypeCode === financialEntityRelTypeCode);
        if (RELATION_INDEX !== -1) {
            this.entityDetailService.availableRelationships.splice(RELATION_INDEX, 1);
        }
        if (this.entityDetailService.availableRelationships.length) {
            this.entityDetailService.groupedRelations = this.groupBy(deepCloneObject(this.entityDetailService.availableRelationships), "coiDisclosureType", "description");
        }
    }

    groupBy(jsonData, key, innerKey) {
        return jsonData.reduce((relationsTypeGroup, item) => {
            (relationsTypeGroup[item[key][innerKey]] = relationsTypeGroup[item[key][innerKey]] || []).push(item);
            return relationsTypeGroup;
        }, {});
    }

    async getRelationshipLookUp(): Promise<any> {
        try {
            const response = await this.entityDetailService.addSFILookUp();
            return response;
        } catch (error) {
            this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
        }
    }

    relationDetailsLeavePage() {
        this.entityDetailService.isAdditionalDetailsChanged = false;
        let index = this.entityDetailService.unSavedSections.findIndex(ele => ele.includes(SFI_ADDITIONAL_DETAILS_SECTION_NAME));
        if (index >= 0) {
            this.entityDetailService.unSavedSections.splice(index, 1);
        }
        hideModal('relationDetailsUnSavedChanges');
        if (this.entityDetailService.clickedTab === 'HISTORY') {
            this.entityDetailService.selectedTab = 'HISTORY';
        } else if (this.entityDetailService.clickedTab === 'QUESTIONNAIRE') {
            this.openQuestionnaire(this.entityDetailService.currentRelationshipQuestionnaire);
        } 
    }

    clearModal() {
        this.relationValidationMap.clear();
        this.entityDetailService.isChecked = {};
    }

    triggerAddRelation() {
        this.addRelation();
    }

    addRelation() {
        if (!this.isSaving && this.validateRelationship()) {
            this.isSaving = true;
            const REQ_BODY = {
                'questionnaireAnsHeaderId': null,
                'personEntityId': this._route.snapshot.queryParamMap.get('personEntityId'),
                'validPersonEntityRelTypeCodes': this.getSelectedRelationTypeCodes().map(typeCode => Number(typeCode))
            };
            this.$subscriptions.push(this.entityDetailService.saveOrUpdateCoiFinancialEntityDetails(REQ_BODY).subscribe((res: any) => {
                res.personEntityRelationships.forEach(ele => {
                    this.entityDetailService.definedRelationships.push(ele);
                    this.findRelation(ele.validPersonEntityRelTypeCode);
                });
                this.openQuestionnaire(res.personEntityRelationships[0]);
                this.clearRelationModal();
                this.isSaving = false;
                hideModal('addRelationshipModal');
                this.entityDetailService.$addOrDeleteRelation.next({'action': 'INSERT', 'element': res.personEntityRelationships, 'isFormCompleted': res.isFormCompleted});
            }, error => {
                this.isSaving = false;
                if (error.status === 405) {
                    hideModal('addRelationshipModal');
                    this.entityDetailService.concurrentUpdateAction = 'Add Relationship';
                } else {
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                }
            }));
        }
    }

    clearRelationModal() {
        this.entityDetailService.isChecked = {};
    }

    validateRelationship() {
        this.relationValidationMap.clear();
        if (!this.getSelectedRelationTypeCodes().length) {
            this.relationValidationMap.set('relationRadio', 'Please select a relation to continue.');
        }
        return this.relationValidationMap.size === 0 ? true : false;
    }

    getSelectedRelationTypeCodes() {
        return Object.keys(this.entityDetailService.isChecked).filter(key => this.entityDetailService.isChecked[key]);
    }

    resetServiceValues() {
        this.entityDetailService.activeRelationship = {};
        this.entityDetailService.definedRelationships = [];
        this.entityDetailService.availableRelationships = [];
        this.entityDetailService.relationshipCompletedObject = {};
        this.entityDetailService.currentRelationshipQuestionnaire = {};
        this.entityDetailService.isHoverEntityCard = false;
        this.entityDetailService.canMangeSfi = false;
        this.entityDetailService.selectedTab = 'QUESTIONNAIRE';
        this.entityDetailService.currentVersionDetails = {};
        this.entityDetailService.groupedRelations = {};
    }

}
