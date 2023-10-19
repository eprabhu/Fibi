import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { CommonService } from '../../common/services/common.service';
import { EntityDetail  } from '../../disclosure/sfi/add-sfi.interface';
import { SfiService } from '../../disclosure/sfi/sfi.service';
import { HTTP_ERROR_STATUS } from '../../app-constants';

@Component({
  selector: 'app-add-relationship-modal',
  templateUrl: './add-relationship-modal.component.html',
  styleUrls: ['./add-relationship-modal.component.scss']
})
export class AddRelationshipModalComponent implements OnInit {

  @Output() hideModal = new EventEmitter<boolean>();

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
  isChecked = {};
  RELATION_TITLE_HELP_TEXT = "Select Relationship Details";

  constructor( private _router:Router, private _sfiService: SfiService, private _activatedRoute: ActivatedRoute,
              private _commonService: CommonService) { }

  ngOnInit() {
    this.getRelationshipLookUp();
    this.triggerAddRelation();
  }

  getRelationshipLookUp() {
    this.$subscriptions.push(this._sfiService.addSFILookUp().subscribe((res: any) => {
      this.relationLookup = res.personEntityRelType;
    }));
  }

  addRelation(data = null) {
    this.relationValidationMap.clear();
    if (!this.isSaving && this.validateRelationship()) {
      this.isSaving = true;
      const REQ_BODY = {
        "questionnaireAnsHeaderId": null,
        "personEntityId" : data,
        "validPersonEntityRelTypeCodes": this.getSelectedRelationTypeCodes().map(typeCode => Number(typeCode))
      }
      this.$subscriptions.push(this._sfiService.saveOrUpdateCoiFinancialEntityDetails(REQ_BODY).subscribe((res: any) => {
        res.forEach(ele => {
          this.definedRelationships.push(ele);
          this.findRelation(ele.validPersonEntityRelType.relationshipTypeCode);
        });
        this.clearRelationModal();
        this.isSaving = false;
        this.navigateToSFI(data);
      } , err => {
        this.navigateToSFI(data);
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in adding relationship');
      }));
    }
  }

  getSelectedRelationTypeCodes() {
    return Object.keys(this.isChecked).filter(key => this.isChecked[key]);
  }

  private navigateToSFI(personEntityId) {
    this.hideModal.emit(false);
    this._sfiService.isShowSfiNavBar = false;
    this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: personEntityId, mode: 'edit' } });
  }

  validateRelationship() {
    this.relationValidationMap.clear();
    if (!this.getSelectedRelationTypeCodes().length) {
      this.relationValidationMap.set('relationRadio', 'Please select a relation to continue.');
    }
    return this.relationValidationMap.size === 0 ? true : false;
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
    this.entityDetail.personEntityRelType = null;
    this.entityDetail.validPersonEntityRelTypes.validPersonEntityRelTypeCode = null;
    this.isChecked = {};
  }

  triggerAddRelation() {
    this.$subscriptions.push(this._sfiService.$addRelationService.subscribe((data: any) => {
      if(data) {
        this.addRelationViaSlider(data);
      } else {
        this.isChecked = {};
      }
    }))
  }

  addRelationViaSlider(data) {
    if (this.getSelectedRelationTypeCodes().length == 0) {
      this._router.navigate(['/coi/entity-details/entity'], { queryParams: { personEntityId: data, mode: 'edit' } })
      this._sfiService.isShowSfiNavBar = false;
    } else {
      this.addRelation(data);
    }
  }

}
