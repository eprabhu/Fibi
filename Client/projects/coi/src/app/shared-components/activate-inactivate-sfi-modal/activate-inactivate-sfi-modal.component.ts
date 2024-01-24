import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivateInactivateSfiModalService } from './activate-inactivate-sfi-modal.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { hideModal, openModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { ActivatedRoute, Router } from '@angular/router';

@Component({
    selector: 'app-activate-inactivate-sfi-modal',
    templateUrl: './activate-inactivate-sfi-modal.component.html',
    styleUrls: ['./activate-inactivate-sfi-modal.component.scss'],
    providers: [ActivateInactivateSfiModalService]
})
export class ActivateInactivateSfiModalComponent implements OnInit, OnDestroy {

    constructor(private _activateInactivateSfiService: ActivateInactivateSfiModalService, private _commonServices: CommonService,
        private _router: Router, private _activatedRoute: ActivatedRoute) { }
        
    @Input() entityName: any = {};
    reasonValidateMapSfi = new Map();
    activateInactivateReason = '';
    $subscriptions: Subscription[] = [];
    @Output() closeModal: EventEmitter<any> = new EventEmitter<any>();
    @Input() entityDetails: any;
    @Input() personEntityId: number = null;
    @Input() updatedRelationshipStatus: string;
    @Input() personEntityNumber: any;
    concurrentActionName = '';

    ngOnInit() {
        document.getElementById('activate-inactivate-show-btn').click();
    }

    activateAndInactivateSfi() {
        if (this.validForActivateAndInactivate()) {
            const REQ_BODY = {
                personEntityId: this.personEntityId,
                versionStatus: this.updatedRelationshipStatus,
                revisionReason: this.activateInactivateReason,
                personEntityNumber: this.personEntityNumber
            };
            this.setActivateInactivate(REQ_BODY);
        }
    }

    setActivateInactivate(REQ_BODY) {
        this.$subscriptions.push(this._activateInactivateSfiService.activateAndInactivateSfi(REQ_BODY).subscribe((res: any) => {
            this.activateOrInactivateSuccess(res);
            this._commonServices.showToast(HTTP_SUCCESS_STATUS, `SFI ${this.updatedRelationshipStatus == 'INACTIVE' ? 'inactivated' : 'activated '} successfully`);
        }, err => {
            if (err.status === 405) {
                document.getElementById('activate-inactivate-show-btn').click();
                this.concurrentActionName = this.updatedRelationshipStatus == 'INACTIVE' ? 'Inactivate SFI' : 'Activate SFI';
                openModal('sfiConcurrentActionModalCOI');
            } else {
                this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
                this.activateOrInactivateFailed();
            }
        }));
    }

    closeSfiActivateAndInactivateModal(data = null) {
        this.activateInactivateReason = '';
        this.closeModal.emit(data ? data : false);
    }

    validForActivateAndInactivate(): boolean {
        this.reasonValidateMapSfi.clear();
        if (!this.activateInactivateReason && this.updatedRelationshipStatus == 'INACTIVE') {
            this.reasonValidateMapSfi.set('reason', `* Please provide a reason for inactivation.`);
        }
        return this.reasonValidateMapSfi.size === 0 ? true : false;
    }

    ngOnDestroy() {
        subscriptionHandler(this.$subscriptions);
    }

    activateOrInactivateSuccess(response) {
        this.closeSfiActivateAndInactivateModal(response);
        this._commonServices.showToast(HTTP_SUCCESS_STATUS, `SFI successfully ${this.updatedRelationshipStatus == 'INACTIVE' ? 'inactivated' : 'activated '}`);
        hideModal('activateInactivateSfiModal');
    }

    activateOrInactivateFailed() {
        this.closeSfiActivateAndInactivateModal();
        this._commonServices.showToast(HTTP_ERROR_STATUS, `Error in ${this.updatedRelationshipStatus == 'INACTIVE' ? 'inactivating' : 'activating'} SFI`);
        hideModal('activateInactivateSfiModal');
    }

    navigateConcurrency() {
        if (!this._router.url.includes('entity-details/entity') || this.updatedRelationshipStatus == 'INACTIVE') {
                window.location.reload();
        }
        this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: this.personEntityId, mode: 'view' } });
        hideModal('sfiConcurrentActionModalCOI');
    }
}
