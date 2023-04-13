import { Component, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { EntityDetailsService } from '../entity-details.service';
import { hideModal, openModal } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { CoiFinancialEntityDetail } from '../../sfi/add-sfi.interface';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';

@Component({
  selector: 'app-entity-questionnaire',
  templateUrl: './entity-questionnaire.component.html',
  styleUrls: ['./entity-questionnaire.component.scss']
})
export class EntityQuestionnaireComponent implements OnInit,OnDestroy {

  $externalSaveEvent = new BehaviorSubject<Boolean>(null);
  configuration: any = {
    moduleItemCode: 8,
    moduleSubitemCodes: [801],
    moduleItemKey: '',
    moduleSubItemKey: '',
    actionUserId: this._commonService.getCurrentUserDetail('personID'),
    actionPersonName: this._commonService.getCurrentUserDetail('fullName'),
    enableViewMode: false,
    isChangeWarning: true,
    isEnableVersion: true,
  }
  relationLookup: any = [];
  definedRelationships: any = [];
  isAddRelationButtonToggled = false;
  activeRelationship: any = 0;
  currentSelected = {
    tab: 'Financial'
  }
  isShowRelationshipModal = false;
  coiFinancialEntityDetail: CoiFinancialEntityDetail = new CoiFinancialEntityDetail();
  isSave = false;
  relationValidationMap = new Map();
  isEditMode = false;
  $subscriptions: Subscription[] = [];


  constructor(private _commonService: CommonService, private _router: Router,
    private _entityDetailsServices: EntityDetailsService,
    private _activatedRoute:ActivatedRoute
    ) { }

  ngOnInit() {
    this.isEditMode = this._activatedRoute.snapshot.queryParamMap.get('mode') === 'edit';
    this.getDataFromService();
    this.configuration.enableViewMode = !this.isEditMode;

  }

  ngOnDestroy(){
    subscriptionHandler(this.$subscriptions);
    hideModal('addRelationshipModal');
  }

  getDataFromService() {
    this.relationLookup = this._entityDetailsServices.lookups;
    this.getDefinedRelationships()
    if (this.definedRelationships.length > 0) {
      this.getQuestionnaire(this.definedRelationships[0]);
    }
    // console.log(this.relationLookup);

    this.removeExistingRelation()
  }

  getSaveEvent(_event) {
    // this.relationLookup.length ? this.addRelations() : this.navigateBack();
    this.$externalSaveEvent.next(true);
  }
  addRelations(flag = false) {
    this.isAddRelationButtonToggled = flag;
  }

  navigateBack() {
    this._router.navigateByUrl(this._entityDetailsServices.previousURL);
  }
  getDefinedRelationships() {
    this.$subscriptions.push(this._entityDetailsServices.$entityDetails.subscribe((res: any) => {
      this.configuration.moduleItemKey = res.coiFinancialEntity.coiFinancialEntityId;
      this.definedRelationships = res.coiFinancialEntityDetails;
      this.coiFinancialEntityDetail.coiFinancialEntityId = res.coiFinancialEntity.coiFinancialEntityId;
    }));
  }

  getQuestionnaire(data: any) {
    this.activeRelationship = data.financialEntityRelTypeCode;
    this.configuration.moduleSubItemKey = data.financialEntityRelTypeCode;
    this.configuration = Object.assign({}, this.configuration);
  }

  addRelation() {
    this.isSave = true;
    if(this.isSave && this.validateRelationship()){
     this.$subscriptions.push(this._entityDetailsServices.saveOrUpdateCoiFinancialEntityDetails({
        coiFinancialEntityDetail:
          this.coiFinancialEntityDetail
      }).subscribe((res: any) => {
        this.definedRelationships.push(res);
        this.getQuestionnaire(res.coiFinancialEntityRelType);
        this.findRelation(res.financialEntityRelTypeCode);
        this.clearRelationModal();
        this.isSave = false;
      }));
    } else {
      hideModal('addRelationshipModal');
    }
  }

  closeModal(elementId) {
    hideModal(elementId);
  }

  openAddRelationshipModal(elementId) {
    openModal(elementId);
  }

  setRelationship() {
    this.coiFinancialEntityDetail.coiFinancialEntityRelType = this.relationLookup.find(
      ele => ele.financialEntityRelTypeCode == this.coiFinancialEntityDetail.financialEntityRelTypeCode
    );
  }



  private findRelation(financialEntityRelTypeCode: string) {
    const RELATION_INDEX = this.relationLookup.findIndex(element => element.financialEntityRelTypeCode === financialEntityRelTypeCode);
    if (RELATION_INDEX !== -1) {
      this.relationLookup.splice(RELATION_INDEX, 1);
    }
  }

  clearRelationModal() {
    hideModal('addRelationshipModal');
    this.coiFinancialEntityDetail.coiFinancialEntityRelType = null;
    this.coiFinancialEntityDetail.financialEntityRelTypeCode = null;
  }
  private removeExistingRelation() {
    if (this.definedRelationships.length) {
      this.activeRelationship = this.definedRelationships[0].financialEntityRelTypeCode;
      this.definedRelationships.forEach(element => {
        this.findRelation(element.financialEntityRelTypeCode);
      });
    }
  }
  validateRelationship() {
    this.relationValidationMap.clear();
    if (!this.coiFinancialEntityDetail.financialEntityRelTypeCode) {
        this.relationValidationMap.set('relationRadio', 'Please select a relation to continue.');
    }
    return this.relationValidationMap.size === 0 ? true : false;
}

}
