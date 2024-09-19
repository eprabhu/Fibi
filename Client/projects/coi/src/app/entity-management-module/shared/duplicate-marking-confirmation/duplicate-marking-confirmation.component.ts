import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { DuplicateMarkingAPIReq, DupMarkingModalConfig, EntityCardDetails, EntityDetails, EntityDetailsInPopup, removeToast } from '../entity-interface';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { DuplicateMarkingConfirmationService } from './duplicate-marking-confirmation.service';
import { ActivatedRoute } from '@angular/router';
import { closeCommonModal, isEmptyObject, openCommonModal } from '../../../common/utilities/custom-utilities';
import { CommonService } from '../../../common/services/common.service';
import { COMMON_ERROR_TOAST_MSG, ENTITY_DOCUMNET_STATUS_TYPE, HTTP_ERROR_STATUS } from '../../../app-constants';
import { EntityDataStoreService } from '../../entity-data-store.service';
import { getEntityFullAddress } from '../../entity-management.service';

@Component({
    selector: 'app-duplicate-marking-confirmation',
    templateUrl: './duplicate-marking-confirmation.component.html',
    styleUrls: ['./duplicate-marking-confirmation.component.scss'],
    providers: [DuplicateMarkingConfirmationService]
})

export class DuplicateMarkingConfirmationComponent implements OnInit, OnChanges, OnDestroy {

    @Input() dupMarkingModalConfig = new DupMarkingModalConfig();
    @Input() duplicateEntityDetails = new EntityCardDetails();
    @Output() emitAPISuccess = new EventEmitter<boolean>();

    $subscriptions: Subscription[] = [];
    entityCardDetails = new EntityDetailsInPopup();
    description: string;
    mandatoryList = new Map();
    entityDetails: EntityDetails = new EntityDetails();
    ENTITY_DUPLICATE_MARKING_MODAL_ID = 'entity_duplicate_marking';
    originalEntityName: string;
    isSaving = false;

    constructor(private _dupMarkingService: DuplicateMarkingConfirmationService,
        private _route: ActivatedRoute, private _commonService: CommonService, private _dataStorService: EntityDataStoreService) { }

    ngOnInit(): void {
        this.listenDataChangeFromStore();
        this.getDataFromStore();
    }

    ngOnChanges(): void {
        if (this.duplicateEntityDetails && !isEmptyObject(this.duplicateEntityDetails) && this.duplicateEntityDetails.entityId) {
            this.entityCardDetails.entityName = this.duplicateEntityDetails.entityName;
            this.entityCardDetails.email = this.duplicateEntityDetails.email;
            this.entityCardDetails.phone = this.duplicateEntityDetails.phoneNumber;
            this.entityCardDetails.website = this.duplicateEntityDetails.website;
            this.entityCardDetails.entityId = this.duplicateEntityDetails.entityId;
            this.entityCardDetails.fullAddress = getEntityFullAddress(this.duplicateEntityDetails);
            this.description = '';
            this.mandatoryList.clear();
            openCommonModal(this.ENTITY_DUPLICATE_MARKING_MODAL_ID);
        }
    }

    checkMandatory(): void {
        this.mandatoryList.clear();
        if (!this.description) {
            this.mandatoryList.set('description', 'Please enter the description.');
        }
    }

    setEntityAsDuplicate(): void {
        this.checkMandatory();
        if (!this.mandatoryList.size && !this.isSaving) {
            this.isSaving = true;
            this.$subscriptions.push(this._dupMarkingService.markAsDuplicate(this.getRequestObj()).subscribe((data: any) => {
                this.entityDetails.entityDocumentStatusType.documentStatusTypeCode = '3';
                this.entityDetails.entityDocumentStatusType.description = 'Duplicate';
                this.entityDetails.originalEntityId = this.duplicateEntityDetails.entityId;
                this.originalEntityName = this.duplicateEntityDetails.entityName;
                this.isSaving = false;
                removeToast('ERROR');
                removeToast('SUCCESS');
                this._dataStorService.updateStore(['entityDetails', 'originalName'], { entityDetails: this.entityDetails, originalName: this.originalEntityName });
                this.clearValuesAndCloseModal();
                this.emitAPISuccess.emit(true);
            }, err => {
                this.isSaving = false;
                this._commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
            }
            ));
        }
    }

    clearValuesAndCloseModal(): void {
        closeCommonModal(this.ENTITY_DUPLICATE_MARKING_MODAL_ID);
        this.description = '';
        this.mandatoryList.clear();
    }

    getRequestObj(): DuplicateMarkingAPIReq {
        const REQOBJ = new DuplicateMarkingAPIReq();
        REQOBJ.originalEntityId = this.duplicateEntityDetails.entityId;
        REQOBJ.duplicateEntityId = this._route.snapshot.queryParamMap.get('entityManageId');
        REQOBJ.description = this.description;
        return REQOBJ;
    }

    private getDataFromStore(): any {
        const ENTITY_DATA = this._dataStorService.getData();
        if (ENTITY_DATA && !isEmptyObject(ENTITY_DATA)) {
            this.entityDetails = ENTITY_DATA?.entityDetails;
            this.originalEntityName = ENTITY_DATA?.originalName;
        }
    }

    private listenDataChangeFromStore(): void {
        this.$subscriptions.push(
            this._dataStorService.dataEvent.subscribe((dependencies: string[]) => {
                this.getDataFromStore();
            })
        );
    }

    ngOnDestroy(): void {
        subscriptionHandler(this.$subscriptions);
    }

}
