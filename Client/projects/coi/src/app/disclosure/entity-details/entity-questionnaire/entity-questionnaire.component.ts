import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { EntityDetailsService } from '../entity-details.service';
import { hideModal, openModal } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { EntityDetail } from '../../sfi/add-sfi.interface';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS } from '../../../app-constants';

@Component({
  selector: 'app-entity-questionnaire',
  templateUrl: './entity-questionnaire.component.html',
  styleUrls: ['./entity-questionnaire.component.scss']
})
export class EntityQuestionnaireComponent implements OnInit, OnDestroy, OnChanges {

  $externalSaveEvent = new BehaviorSubject<Boolean>(null);
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
  };
  relationLookup: any = [];
  definedRelationships: any = [];
  isAddRelationButtonToggled = false;
  activeRelationship: any = 0;
  currentSelected = {
    tab: 'FINANCIAL'
  };
  isShowRelationshipModal = false;
  coiFinancialEntityDetail: EntityDetail = new EntityDetail();
  isSaving = false;
  relationValidationMap = new Map();
  isEditMode = false;
  $subscriptions: Subscription[] = [];
  @Output() updateRelationship: EventEmitter<any> = new EventEmitter<any>();
  @Input() isAddRelationship = false;
  @Output() isEmitModalClose: EventEmitter<boolean> = new EventEmitter<boolean>();
  @Output() positionsToView: EventEmitter<boolean> = new EventEmitter<boolean>();
  @Input() entityId: any;

  constructor(private _commonService: CommonService, private _router: Router,
    public entityDetailsServices: EntityDetailsService,
    private _activatedRoute: ActivatedRoute
  ) { }

  ngOnInit() {
    this.isEditMode = this._activatedRoute.snapshot.queryParamMap.get('mode') === 'edit';
    this.$subscriptions.push(this._activatedRoute.queryParams.subscribe(params => {
      this.getDataFromService();
    }));
    this.configuration.enableViewMode = !this.isEditMode;
  }
  ngOnChanges() {
    if (this.isAddRelationship) {
      this.openAddRelationshipModal('addRelationshipModal');
    }

  }
  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
    hideModal('addRelationshipModal');
  }

  async getDataFromService() {
    this.relationLookup = await this.getRelationshipLookUp();
    await this.getDefinedRelationships();
    if (this.definedRelationships.length > 0) {
      this.getQuestionnaire(this.definedRelationships[0]);
    }
    this.removeExistingRelation();
  }

  async getRelationshipLookUp(): Promise<any> {
    try {
      const response = await this.entityDetailsServices.addSFILookUp(this.currentSelected.tab);
      return response.validPersonEntityRelTypes;
    } catch (error) {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
    }
  }

  addRelations(flag = false) {
    this.isAddRelationButtonToggled = flag;
  }

  navigateBack() {
    this._router.navigateByUrl(this.entityDetailsServices.previousURL);
  }
  getDefinedRelationships() {
    const REQ_BODY = {
      'tabName': this.currentSelected.tab,
      'personEntityId': this._activatedRoute.snapshot.queryParamMap.get('personEntityId') || this.entityId
    };
    return new Promise<boolean>((resolve) => {
      this.$subscriptions.push(this.entityDetailsServices.getPersonEntityRelationship(REQ_BODY).subscribe((res: any) => {
        this.configuration.moduleItemKey = this._activatedRoute.snapshot.queryParamMap.get('personEntityId') || this.entityId;
        this.definedRelationships = res.personEntityRelationships;
        (this.isEditMode && this.definedRelationships.length > 0) ? this.positionsToView.emit(true) : this.positionsToView.emit(false);
        this.removeExistingRelation();
        resolve(true);
      }, error => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
      }));
    });
  }

  getQuestionnaire(data: any) {
    this.activeRelationship = data.validPersonEntityRelType?.personEntityRelType.relationshipTypeCode;
    this.configuration.moduleSubItemKey = data.validPersonEntityRelTypeCode;
    this.configuration = Object.assign({}, this.configuration);
  }

  addRelation() {
    if (!this.isSaving && this.validateRelationship()) {
      this.isSaving = true;
      const REQ_BODY = {
        'questionnaireAnsHeaderId': null,
        'personEntityId': this._activatedRoute.snapshot.queryParamMap.get('personEntityId'),
        'validPersonEntityRelTypeCode': this.coiFinancialEntityDetail.validPersonEntityRelTypes.relationshipTypeCode
      };
      this.$subscriptions.push(this.entityDetailsServices.saveOrUpdateCoiFinancialEntityDetails(REQ_BODY).subscribe((res: any) => {
        this.definedRelationships.push(res);
        this.getQuestionnaire(res);
        this.findRelation(res.validPersonEntityRelType.relationshipTypeCode);
        this.clearRelationModal();
        this.isSaving = false;
        this.updateRelationship.emit(res);
      }, error => {
        this.isSaving = false;
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Something went wrong, Please try again.');
      }));
    }
  }

  closeModal(elementId) {
    hideModal(elementId);
  }

  openAddRelationshipModal(elementId) {
    openModal(elementId);
  }

  setRelationship() {
    this.coiFinancialEntityDetail.personEntityRelType = this.relationLookup.find(
      ele => ele.personEntityRelType.relationshipTypeCode === this.coiFinancialEntityDetail.validPersonEntityRelTypes.relationshipTypeCode);
  }



  private findRelation(financialEntityRelTypeCode: string) {
    const RELATION_INDEX = this.relationLookup.findIndex(element =>
      element.personEntityRelType.relationshipTypeCode === financialEntityRelTypeCode);
    if (RELATION_INDEX !== -1) {
      this.relationLookup.splice(RELATION_INDEX, 1);
    }
  }

  clearRelationModal() {
    hideModal('addRelationshipModal');
    this.coiFinancialEntityDetail.personEntityRelType = null;
    this.coiFinancialEntityDetail.validPersonEntityRelTypes.relationshipTypeCode = null;
  }
  private removeExistingRelation() {
    if (this.definedRelationships.length) {
      this.activeRelationship = this.definedRelationships[0].validPersonEntityRelType.relationshipTypeCode;
      this.definedRelationships.forEach(element => {
        this.findRelation(element.validPersonEntityRelType.personEntityRelType.relationshipTypeCode);
      });
    }
  }
  validateRelationship() {
    this.relationValidationMap.clear();

    if (!this.coiFinancialEntityDetail.validPersonEntityRelTypes.relationshipTypeCode) {
      this.relationValidationMap.set('relationRadio', 'Please select a relation to continue.');
    }
    return this.relationValidationMap.size === 0 ? true : false;
  }

  clearModal() {
    this.relationValidationMap.clear();
    this.isEmitModalClose.emit(false);
  }

  questionnaireSaveAction(event) {
    this.entityDetailsServices.$saveQuestionnaireAction.next(event);
  }

}
