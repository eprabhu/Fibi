import { DuplicateEntityCheckService } from './duplicate-entity-check.service';
import { Component, EventEmitter, Input, OnChanges, OnDestroy, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { DuplicateCheckObj, ENTITY_DUPLICATE_MATCH_MODAL_ID, EntityCardDetails, EntityDupCheckConfig} from '../entity-interface';
import { CommonService } from '../../../common/services/common.service';
import { COMMON_ERROR_TOAST_MSG, HTTP_ERROR_STATUS } from '../../../app-constants';
import { COIModalConfig, ModalActionEvent } from '../../../shared-components/coi-modal/coi-modal.interface';
import { closeCommonModal, isEmptyObject, openCommonModal } from '../../../common/utilities/custom-utilities';
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
    @Output() actionResponse = new EventEmitter<'CLOSE_BTN' | 'SECONDARY_BTN' | 'PRIMARY_BTN' | 'NOT_FOUND'>();

    $subscriptions: Subscription[] = [];
    matchedDuplicateEntites: EntityCardDetails[] = [];
    duplicateEntityModalConfig = new COIModalConfig(ENTITY_DUPLICATE_MATCH_MODAL_ID, this.entityDupCheckConfig.modalPrimaryButton, 'Cancel', 'xl');

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
                this.matchedDuplicateEntites.map(ele => ele.primaryAddress = ele.primaryAddressLine1 + (ele.primaryAddressLine2 ? ',' + ele.primaryAddressLine2 : '') );
                if (this.entityDupCheckConfig.duplicateView === 'MODAL_VIEW') {
                    openCommonModal(ENTITY_DUPLICATE_MATCH_MODAL_ID);
                }
            } else {
                this.actionResponse.emit('NOT_FOUND');
            }
        }, error => {
            this._commonService.showToast(HTTP_ERROR_STATUS, COMMON_ERROR_TOAST_MSG);
        }
        ));
    }

    modalAction(event: ModalActionEvent) {
        this.actionResponse.emit(event.action);
        closeCommonModal(ENTITY_DUPLICATE_MATCH_MODAL_ID);
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

}
