import {Component, Input, OnDestroy, OnInit} from '@angular/core';
import {EntityExternalIdMappings, OtherReferenceId} from '../../shared/entity-interface';
import {hideModal, isEmptyObject, openModal} from 'projects/fibi/src/app/common/utilities/custom-utilities';
import {Subscription} from 'rxjs';
import {subscriptionHandler} from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import {EntityOverviewService} from '../entity-overview.service';
import {HTTP_ERROR_STATUS} from '../../../app-constants';
import {CommonService} from '../../../common/services/common.service';
import {EntityDataStoreService} from '../../entity-data-store.service';
import {COIModalConfig, ModalActionEvent} from "../../../shared-components/coi-modal/coi-modal.interface";
import {closeCommonModal, openCommonModal} from "../../../common/utilities/custom-utilities";

@Component({
    selector: 'app-other-reference-id',
    templateUrl: './other-reference-id.component.html',
    styleUrls: ['./other-reference-id.component.scss']
})
export class OtherReferenceIdComponent implements OnInit, OnDestroy {

    otherReferenceIdObj: OtherReferenceId = new OtherReferenceId();
    coiCurrencyOptions = 'entity_external_id_type#EXTERNAL_ID_TYPE_CODE#false#false';
    @Input() sectionName: any;
    @Input() sectionId: any;
    mandatoryList = new Map();
    externalReferences: EntityExternalIdMappings[] = [];
    selectedReference = null;
    isEditIndex = null;
    isEditMode = false;
    isSaving = false;
    entityId = null;
    defaultRefType = null;
    selectedRefType = [];
    $subscriptions: Subscription[] = [];
    CONFIRMATION_MODAL_ID = 'ref-delete-confirm-modal';
    modalConfig = new COIModalConfig(this.CONFIRMATION_MODAL_ID, 'Delete', 'Cancel');

    constructor(private _entityOverviewService: EntityOverviewService,
                private _commonService: CommonService,
                private _dataStoreService: EntityDataStoreService) {
    }

    ngOnInit() {
        this.getDataFromStore();
        this.listenDataChangeFromStore();
    }

    listenDataChangeFromStore() {
        this.$subscriptions.push(
            this._dataStoreService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    private getDataFromStore() {
        const entityData = this._dataStoreService.getData();
        if (isEmptyObject(entityData)) {
            return;
        }
        this.entityId = entityData?.entityDetails?.entityId;
        this.externalReferences = entityData.entityExternalIdMappings;
        this.isEditMode = this._dataStoreService.getEditMode();
    }


    addOtherRefId(event) {
        openModal('otherReferenceIdModal');
    }

    onReferenceIdTypeSelected(event) {
        if (event && event.length) {
            this.otherReferenceIdObj.externalIdTypeCode = event[0].code;
            this.selectedReference = event[0];
        } else {
            this.otherReferenceIdObj.externalIdTypeCode = null;
            this.selectedReference = null;
        }
    }

    addOtherReferenceID() {
        if (this.entityMandatoryValidation()) {
            this.isEditIndex != null ? this.updateExternalReference() : this.addExternalReference();
        }
    }

    clearOtherReferenceID() {
        this.mandatoryList.clear();
        this.otherReferenceIdObj = new OtherReferenceId();
        this.defaultRefType = null;
        this.isEditIndex = null;
        this.selectedRefType = [];
        hideModal('otherReferenceIdModal');
    }

    entityMandatoryValidation() {
        this.mandatoryList.clear();
        if (!this.otherReferenceIdObj.externalIdTypeCode) {
            this.mandatoryList.set('referenceType', 'Please select reference type.');
        }
        if (!this.otherReferenceIdObj.externalId) {
            this.mandatoryList.set('referenceId', 'Please enter reference id.');
        }
        if (!this.otherReferenceIdObj.description) {
            this.mandatoryList.set('description', 'Please enter reference description.');
        }
        return !this.mandatoryList.size;
    }

    addExternalReference() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.otherReferenceIdObj.entityId = this.entityId;
            this.$subscriptions.push(this._entityOverviewService.saveExternalReference(this.otherReferenceIdObj).subscribe((res: any) => {
                const newReference: any = this.otherReferenceIdObj;
                newReference.entityExternalMappingId = res.entityExternalMappingId;
                newReference.entityExternalIdType = this.selectedReference;
                this.externalReferences.push(newReference);
                this.updateDataStore();
                this.clearOtherReferenceID();
                this.isSaving = false;
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }

    editRef(reference: EntityExternalIdMappings, index: number) {
        this.isEditIndex = index;
        this.setRefObj(reference);
        openModal('otherReferenceIdModal');
    }

    private setRefObj(reference: EntityExternalIdMappings) {
        this.otherReferenceIdObj.entityId = this.entityId;
        this.otherReferenceIdObj.externalId = reference.externalId;
        this.otherReferenceIdObj.description = reference.description;
        this.otherReferenceIdObj.externalIdTypeCode = reference.externalIdTypeCode;
        this.otherReferenceIdObj.entityExternalMappingId = reference.entityExternalMappingId;
        this.defaultRefType = reference.entityExternalIdType.description;
    }

    confirmDeleteRef(reference: EntityExternalIdMappings, index: number) {
        this.isEditIndex = index;
        this.selectedReference = reference;
        openCommonModal(this.CONFIRMATION_MODAL_ID);
    }

    postConfirmation(modalAction: ModalActionEvent) {
        if (modalAction.action == 'PRIMARY_BTN') {
            this.deleteRef();
        }
        closeCommonModal(this.CONFIRMATION_MODAL_ID);
    }

    deleteRef() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._entityOverviewService
                .deleteExternalReference(this.selectedReference.entityExternalMappingId)
                .subscribe((res: any) => {
                    this.externalReferences.splice(this.isEditIndex, 1);
                    this.updateDataStore();
                    this.clearOtherReferenceID();
                    this.isSaving = false;
            }, err => {
                    this.isSaving = false;
                    this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                }));
        }
    }

    updateExternalReference() {
        if (!this.isSaving) {
            this.isSaving = true;
            this.otherReferenceIdObj.entityId = this.entityId;
            this.$subscriptions.push(this._entityOverviewService.updateExternalReference(this.otherReferenceIdObj).subscribe((res: any) => {
                this.updateExternalReferences();
                this.updateDataStore();
                this.clearOtherReferenceID();
                this.isSaving = false;
            }, err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong. Please try again.');
                this.isSaving = false;
            }));
        }
    }

    private updateExternalReferences() {
        this.externalReferences[this.isEditIndex].externalId = this.otherReferenceIdObj.externalId;
        this.externalReferences[this.isEditIndex].description = this.otherReferenceIdObj.description;
        this.setRefIdType();
    }

    setRefIdType() {
        const currentRef = this.externalReferences[this.isEditIndex];
        const selectedRef = this.selectedReference;
        if (selectedRef?.description || currentRef?.entityExternalIdTypeDescription) {
            currentRef.entityExternalIdTypeDescription = selectedRef.description || currentRef.entityExternalIdTypeDescription;
        }
        if (selectedRef || currentRef?.entityExternalIdType) {
            currentRef.entityExternalIdType = selectedRef || currentRef.entityExternalIdType;
        }
        currentRef.externalIdTypeCode =  this.otherReferenceIdObj.externalIdTypeCode;
    }

    updateDataStore() {
        this._dataStoreService.updateStore(['entityExternalIdMappings'], {'entityExternalIdMappings': this.externalReferences});
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }
}
