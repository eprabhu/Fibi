import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { hideModal, openModal } from '../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CommonService } from '../../common/services/common.service';
import { EntityDetail  } from '../../disclosure/sfi/add-sfi.interface';
import { SfiService } from '../../disclosure/sfi/sfi.service';

@Component({
  selector: 'app-add-relationship-modal',
  templateUrl: './add-relationship-modal.component.html',
  styleUrls: ['./add-relationship-modal.component.scss']
})
export class AddRelationshipModalComponent implements OnInit {

  @Output() relationshipResult: EventEmitter<any> = new EventEmitter<any>();
  @Output() hideModal = new EventEmitter<boolean>()
  @Input() isWithOutRelationship = false;
  @Input() personEntityId;

  isAddRelationshipModal: any;
  isSaving = false;
  relationValidationMap = new Map();
  entityDetail: EntityDetail = new EntityDetail ();
  $subscriptions: Subscription[] = [];
  definedRelationships: any = [];
  activeRelationship: any = 0;
  configuration: any = {
    moduleItemCode: 8,
    moduleSubitemCodes: [801],
    moduleItemKey: '',
    moduleSubItemKey: '',
    actionUserId: this._commonService.getCurrentUserDetail('personId'),
    actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
    enableViewMode: false,
    isChangeWarning: true,
    isEnableVersion: true,
  }
  relationLookup: any = [];

  constructor( private _router:Router, private _sfiService: SfiService, private _activatedRoute: ActivatedRoute,
              private _commonService: CommonService) { }

  ngOnInit() {
    openModal('addRelationshipModal');
    this.getRelationshipLookUp();
  }

  closeModal() {
    this.relationshipResult.emit('addRelationshipModal');
    this.hideModal.emit(false);

  }

  getRelationshipLookUp() {
    this.$subscriptions.push(this._sfiService.addSFILookUp().subscribe((res: any) => {
      this.relationLookup = res.personEntityRelType;
    }));
  }

  // addRelation() {
  //   this.entityDetail.validPersonEntityRelTypes.relationshipTypeCode ? this.addRelationAPI() : this.navigateToSFI();
  // }

  addRelation() {
    this.relationValidationMap.clear();
    if (!this.isSaving && this.validateRelationship()) {
      this.isSaving = true;
      const REQ_BODY = {
        "questionnaireAnsHeaderId": null,
        "personEntityId": this.personEntityId,
        "validPersonEntityRelTypeCode": this.entityDetail.validPersonEntityRelTypes.relationshipTypeCode
      }
      this.$subscriptions.push(this._sfiService.saveOrUpdateCoiFinancialEntityDetails(REQ_BODY).subscribe((res: any) => {
        this.definedRelationships.push(res);
        this.clearRelationModal();
        this.isSaving = false;
        this.navigateToSFI();
      }));
    }
  }

  private navigateToSFI() {
    this.relationshipResult.emit('addRelationshipModal');
    this.hideModal.emit(false);
    this._sfiService.isShowSfiNavBar = false;
    this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: this.personEntityId, mode: 'edit' } });
  }

  validateRelationship() {
    this.relationValidationMap.clear();
    if (!this.entityDetail.validPersonEntityRelTypes.relationshipTypeCode) {
      this.relationValidationMap.set('relationRadio', 'Please select a relation to continue.');
    }
    return this.relationValidationMap.size === 0 ? true : false;
  }

  continueWithoutRelation() {
    this.clearRelationModal();
    this.closeModal();
    this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: this.personEntityId, mode: 'edit' } })
    this._sfiService.isShowSfiNavBar = false;
  }

  private findRelation(financialEntityRelTypeCode: string) {
    const RELATION_INDEX = this.relationLookup.findIndex(element => element.validPersonEntityRelTypeCode === financialEntityRelTypeCode);
    if (RELATION_INDEX !== -1) {
      this.relationLookup.splice(RELATION_INDEX, 1);
    }
  }

  setRelationship() {
    this.entityDetail.personEntityRelType = this.relationLookup.find(
      ele => ele.relationshipTypeCode === this.entityDetail.validPersonEntityRelTypes.relationshipTypeCode);
  }

  clearRelationModal() {
    hideModal('addRelationshipModal');
    this.entityDetail.personEntityRelType = null;
    this.entityDetail.validPersonEntityRelTypes.validPersonEntityRelTypeCode = null;
  }

  closeRelationModal() {
    this._sfiService.$addSfi.next(true);
    this.clearRelationModal();
    this.closeModal();
    this._sfiService.isShowSfiNavBar = false;
    this.relationValidationMap.clear();
  }
}
