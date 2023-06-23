import { Component, EventEmitter, Input, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivateInactivateSfiModalService } from './activate-inactivate-sfi-modal.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';

@Component({
  selector: 'app-activate-inactivate-sfi-modal',
  templateUrl: './activate-inactivate-sfi-modal.component.html',
  styleUrls: ['./activate-inactivate-sfi-modal.component.scss'],
  providers: [ActivateInactivateSfiModalService]
})
export class ActivateInactivateSfiModalComponent implements OnInit, OnDestroy {

  constructor(private _activateInactivateSfiService: ActivateInactivateSfiModalService, private _commonServices: CommonService) { }
  @Input() sfiDetails: any = {};
  reasonValidateMapSfi = new Map();
  activateInactivateReason = '';
  $subscriptions: Subscription[] = [];
  @Output() closeModal: EventEmitter<any> = new EventEmitter<any>();
  @Input() personEntityId: number = null;
  @Input() isRelationshipActive = false;

  ngOnInit () {
    document.getElementById('activate-inactivate-show-btn').click();
  }

  activateAndInactivateSfi() {
    const REQ_BODY = {
      personEntityId: this.personEntityId,
      isRelationshipActive: !this.isRelationshipActive,
      revisionReason: this.activateInactivateReason
    };
    this.$subscriptions.push(this._activateInactivateSfiService.activateAndInactivateSfi(REQ_BODY).subscribe((res: any) => {
      this.closeSfiActivateAndInactivateModal(res);
      this._commonServices.showToast(HTTP_SUCCESS_STATUS, `SFI successfully ${this.isRelationshipActive ? 'inactivated' : 'activated '}`);
    }, err => {
      this._commonServices.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }));
  }

  closeSfiActivateAndInactivateModal (data) {
    this.activateInactivateReason = '';
    this.closeModal.emit(data ? data : false);
  }

  validForActivateAndInactivate(): boolean {
    if (!this.activateInactivateReason && this.isRelationshipActive) {
        this.reasonValidateMapSfi.set('reason', `*Please provide a reason for inactivation.`);
    }
    return this.reasonValidateMapSfi.size === 0 ? true : false;
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }
}
