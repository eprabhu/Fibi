import { DuplicateEntityCheckService } from './duplicate-entity-check.service';
import { Component, EventEmitter, Input, OnChanges, OnDestroy, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { DuplicateActionType, DuplicateCheckObj, ENTITY_DUPLICATE_MATCH_MODAL_ID, ENTITY_DUPLICATE_MATCH_SLIDER_ID, EntityCardDetails, EntityDupCheckConfig} from '../entity-interface';
import { CommonService } from '../../../common/services/common.service';
import { COMMON_ERROR_TOAST_MSG, HTTP_ERROR_STATUS } from '../../../app-constants';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { closeCoiSlider, closeCommonModal, isEmptyObject, openCoiSlider, openCommonModal } from '../../../common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';

@Component({
    selector: 'app-duplicate-entity-check',
    templateUrl: './duplicate-entity-check.component.html',
    styleUrls: ['./duplicate-entity-check.component.scss'],
    providers: [DuplicateEntityCheckService]
})
export class DuplicateEntityCheckComponent implements OnChanges, OnDestroy {

    @Input() entityDupCheckConfig = new EntityDupCheckConfig();
    @Input() dupCheckPayload = new DuplicateCheckObj();
    @Output() actionResponse = new EventEmitter<{action: DuplicateActionType, event?: any}>();
    @Output() openModal = new EventEmitter<{action: 'OPEN_MODAL' , event?: any}>();

    $subscriptions: Subscription[] = [];
    matchedDuplicateEntites: EntityCardDetails[] = [];
    ENTITY_DUPLICATE_MATCH_SLIDER_ID = ENTITY_DUPLICATE_MATCH_SLIDER_ID;
    duplicateEntityModalConfig = new COIModalConfig(ENTITY_DUPLICATE_MATCH_MODAL_ID, this.entityDupCheckConfig.primaryButton, 'Cancel', 'xl');

    constructor(private _duplicateCheckService: DuplicateEntityCheckService, private _commonService: CommonService) { }

    ngOnChanges() {
        if (this.dupCheckPayload && !isEmptyObject(this.dupCheckPayload)) {
            this.performDuplicateCheck();
        }
    }
    private performDuplicateCheck(): void {
        if(!this.dupCheckPayload.primaryAddressLine2) {
            delete this.dupCheckPayload['primaryAddressLine2'];
        }
        this.$subscriptions.push(this._duplicateCheckService.checkForDuplicate(this.dupCheckPayload).subscribe((data: EntityCardDetails[] = []) => {
            if (data?.length) {
                this.matchedDuplicateEntites = data;
                if (this.entityDupCheckConfig?.entityIdToFilter) {
                    this.matchedDuplicateEntites = this.matchedDuplicateEntites?.filter((entity: EntityCardDetails) => entity?.entityId != this.entityDupCheckConfig?.entityIdToFilter);
                }
                this.matchedDuplicateEntites.map(ele => ele.primaryAddress = ele.primaryAddressLine1 + (ele.primaryAddressLine2 ? ',' + ele.primaryAddressLine2 : '') );
                if (this.entityDupCheckConfig.duplicateView === 'MODAL_VIEW') {
                    openCommonModal(ENTITY_DUPLICATE_MATCH_MODAL_ID);
                } else if (this.entityDupCheckConfig.duplicateView === 'SLIDER_VIEW') {
                    openCoiSlider(ENTITY_DUPLICATE_MATCH_SLIDER_ID);
                }
            } else {
                this.actionResponse.emit({ action: 'NOT_FOUND' });
            }
        }, error => {
            this.actionResponse.emit({ action: 'API_FAILED' });
            this._commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
        }
        ));
    }

    modalAction(event: ModalActionEvent) {
        this.actionResponse.emit({ action: event.action });
        closeCommonModal(ENTITY_DUPLICATE_MATCH_MODAL_ID);
    }

    closeDuplicateSlider(action: DuplicateActionType, event?: any): void {
        setTimeout(() => {
            this.clearDuplicateSlider(action, event);
        }, 200);
    }

    private clearDuplicateSlider(action: DuplicateActionType, event?: any): void {
        this.matchedDuplicateEntites = [];
        this.actionResponse.emit({ action, event});
    }

    updateVerifyButtonState(): void {
        if (this.entityDupCheckConfig.hasConfirmedNoDuplicate) {
            closeCoiSlider(ENTITY_DUPLICATE_MATCH_SLIDER_ID);
            this.closeDuplicateSlider('CHECK_BOX', this.entityDupCheckConfig);
        }
    }

    openConfirmationModal(action: 'USE' | 'OPEN_MODAL', entity: EntityCardDetails) {
        if(action === 'OPEN_MODAL') {
            this.openModal.emit({action: action, event: entity});
        }
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

}
