import { Component, EventEmitter, Input, OnDestroy, OnInit, Output, ViewChild } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EntityDetailsService, groupBy } from './entity-details.service';
import { deepCloneObject, hideModal, isEmptyObject, openModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
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
    @Input() entityId: any;
    @Output() closeAction: EventEmitter<boolean> = new EventEmitter<boolean>();

    @ViewChild(ViewRelationshipDetailsComponent) viewRelationComponent: ViewRelationshipDetailsComponent;

    isTriggeredFromSlider = false;
    $subscriptions: Subscription[] = [];
    questionnaireSection: any = '';
    relationValidationMap = new Map();
    entityNumber: any;
    isSaving = false;
    checkedRelationships = {};
    SFI_ADDITIONAL_DETAILS_SECTION_NAME = SFI_ADDITIONAL_DETAILS_SECTION_NAME;

    constructor(public entityDetailService: EntityDetailsService, private _route: ActivatedRoute, private _router: Router,
        private _commonService: CommonService, private _navigationService: NavigationService) {}

    async ngOnInit() {
        this.resetServiceValues();
        this.entityDetailService.activeTab = 'QUESTIONNAIRE';
        this.isTriggeredFromSlider = this.checkForSFIOpenedFromSlider();
        this.getQueryParams();
        await this.getDefinedRelationships();
        this.getAvailableRelationship();
        this.listenToAddRelationModal();
        this.listenToLeaveConfirmationModal();
    }

    getQueryParams() {
        this.$subscriptions.push(this._route.queryParams.subscribe(params => {
            this.entityId = params['personEntityId'] || this.entityId;
            this.entityNumber = params['personEntityNumber'] || null;
        }));
    }

    checkForSFIOpenedFromSlider() {
        return ['create-disclosure', 'user-dashboard/entities', 'disclosure/summary', 'entity-management/entity-details', 'user-dashboard/disclosures', 'coi/admin-dashboard'].some(ele => this._router.url.includes(ele))
    }

    async getAvailableRelationship() {
        this.entityDetailService.allAvailableRelationships = await this.getRelationshipLookUp();
        this.entityDetailService.remainingRelationships = deepCloneObject(this.entityDetailService.allAvailableRelationships);
        this.removeExistingRelation();
    }

    /*
    remove already added relationships from available relationship and grouping remaining relations.
    */
    private removeExistingRelation() {
        this.entityDetailService.groupedRelations = {};
        if (this.entityDetailService.definedRelationships.length) {
            this.entityDetailService.definedRelationships.forEach(element => {
                this.findRelationAndRemove(element.validPersonEntityRelType.validPersonEntityRelTypeCode);
            });
        } else {
            if (this.entityDetailService.remainingRelationships.length) {
                this.entityDetailService.groupedRelations = groupBy(deepCloneObject(this.entityDetailService.remainingRelationships), "coiDisclosureType", "description");
            }
        }
    }

    getDefinedRelationships() {
        const REQ_BODY = {
            'personEntityId': this.entityId
        };
        return new Promise<boolean>((resolve) => {
            this.$subscriptions.push(this.entityDetailService.getPersonEntityRelationship(REQ_BODY).subscribe((res: any) => {
                if (res.length) {
                    this.entityDetailService.definedRelationships = res || [];
                } else {
                    this.entityDetailService.activeTab = 'RELATION_DETAILS';
                }
                resolve(true);
            }, error => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                resolve(false);
            }));
        });
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
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

    listenToLeaveConfirmationModal() {
        this.$subscriptions.push(this.entityDetailService.$emitUnsavedChangesModal.subscribe((data: any) => {
            if (this.entityDetailService.isRelationshipQuestionnaireChanged) {
                this.questionnaireSection = this.entityDetailService.unSavedSections.find(ele => ele.includes('Relationship Questionnaire'));
                openModal('questionnaireUnsavedChanges');
            }
            if (this.entityDetailService.isAdditionalDetailsChanged) {
                openModal('relationDetailsUnSavedChanges');
            }
        }));
    }

    openQuestionnaire(entityDetails) {
        this.entityDetailService.activeTab = 'QUESTIONNAIRE';
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

    listenToAddRelationModal() {
        this.$subscriptions.push(this.entityDetailService.$triggerAddRelationModal.subscribe(async (data: any) => {
            this.removeExistingRelation();
            if (this.entityDetailService.isRelationshipQuestionnaireChanged) {
                this.entityDetailService.globalSave$.next();
            }
            this.relationValidationMap.clear();
            openModal('addRelationshipModal');
        }))
    }

    private findRelationAndRemove(financialEntityRelTypeCode: string) {
        this.entityDetailService.groupedRelations = {};
        const RELATION_INDEX = this.entityDetailService.remainingRelationships.findIndex(element =>
            element.validPersonEntityRelTypeCode === financialEntityRelTypeCode);
        if (RELATION_INDEX !== -1) {
            this.entityDetailService.remainingRelationships.splice(RELATION_INDEX, 1);
        }
        if (this.entityDetailService.remainingRelationships.length) {
            this.entityDetailService.groupedRelations = groupBy(deepCloneObject(this.entityDetailService.remainingRelationships), "coiDisclosureType", "description");
        }
    }

    async getRelationshipLookUp(): Promise<any> {
        try {
            return await this.entityDetailService.addSFILookUp();
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
        if (this.entityDetailService.toBeActiveTab === 'HISTORY') {
            this.entityDetailService.activeTab = 'HISTORY';
        } else if (this.entityDetailService.toBeActiveTab === 'QUESTIONNAIRE') {
            this.openQuestionnaire(!isEmptyObject(this.entityDetailService.currentRelationshipQuestionnaire) ? this.entityDetailService.currentRelationshipQuestionnaire : this.entityDetailService.definedRelationships[0]);
        } else if (this.entityDetailService.isVersionChange) {
            this.viewRelationComponent.loadCurrentVersion();
        }
        this.entityDetailService.isVersionChange = false;
    }

    questionnaireChangeModalLeaveTab() {
        this.entityDetailService.isRelationshipQuestionnaireChanged = false;
        let index = this.entityDetailService.unSavedSections.findIndex(ele => ele.includes('Relationship Questionnaire'));
        if (index >= 0) {
            this.entityDetailService.unSavedSections.splice(index, 1);
        }
        if (this.entityDetailService.toBeActiveTab === 'RELATION_DETAILS') {
            this.entityDetailService.activeTab = 'RELATION_DETAILS';
        } else if (this.entityDetailService.toBeActiveTab === 'HISTORY') {
            this.entityDetailService.activeTab = 'HISTORY';
        } else {
           this.entityDetailService.isVersionChange ? this.viewRelationComponent.loadCurrentVersion() : this.openQuestionnaire(this.entityDetailService.currentRelationshipQuestionnaire);
            this.entityDetailService.toBeActiveTab === 'QUESTIONNAIRE';
        }
        this.entityDetailService.isVersionChange = false;
        hideModal('questionnaireUnsavedChanges');
    }

    clearModal() {
        this.relationValidationMap.clear();
        this.checkedRelationships = {};
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
                this.updateNewRelationships(res);
                this.isSaving = false;
                this.checkedRelationships = {};
                hideModal('addRelationshipModal');
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

    async updateNewRelationships(res) {
        this.entityDetailService.currentRelationshipQuestionnaire = res.personEntityRelationships[0];
        if (this.entityId != res.personEntityId) {
            await this.viewRelationComponent.updateModifiedVersion(res, false);
            this.entityDetailService.definedRelationships.forEach(ele => {
                this.findRelationAndRemove(ele.validPersonEntityRelTypeCode);
            });
        } else {
            res.personEntityRelationships.forEach(ele => {
                this.entityDetailService.definedRelationships.push(ele);
                this.findRelationAndRemove(ele.validPersonEntityRelTypeCode);
            });
            this.openQuestionnaire(res.personEntityRelationships[0]);
            this.entityDetailService.$addOrDeleteRelation.next({'element': res.personEntityRelationships, 'isFormCompleted': res.isFormCompleted, 'updateTimestamp': res.updateTimestamp});
        }
    }

    validateRelationship() {
        this.relationValidationMap.clear();
        if (!this.getSelectedRelationTypeCodes().length) {
            this.relationValidationMap.set('relationRadio', 'Please select a relation to continue.');
        }
        return this.relationValidationMap.size === 0 ? true : false;
    }

    getSelectedRelationTypeCodes() {
        return Object.keys(this.checkedRelationships).filter(key => this.checkedRelationships[key]);
    }

    resetServiceValues() {
        this.entityDetailService.activeRelationship = {};
        this.entityDetailService.definedRelationships = [];
        this.entityDetailService.allAvailableRelationships = [];
        this.entityDetailService.remainingRelationships = [];
        this.entityDetailService.relationshipCompletedObject = {};
        this.entityDetailService.currentRelationshipQuestionnaire = {};
        this.entityDetailService.canMangeSfi = false;
        this.entityDetailService.activeTab = 'QUESTIONNAIRE';
        this.entityDetailService.currentVersionDetails = {};
        this.entityDetailService.groupedRelations = {};
    }

}
