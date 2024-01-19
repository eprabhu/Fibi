import { Component, EventEmitter, Input, OnChanges, OnDestroy, OnInit, Output, SimpleChanges } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { BehaviorSubject, Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { EntityDetailsService } from '../entity-details.service';
import { EntityDetail } from '../../sfi/add-sfi.interface';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { deepCloneObject, hideModal, isEmptyObject } from 'projects/fibi/src/app/common/utilities/custom-utilities';

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
  isAddRelationButtonToggled = false;
  currentSelected = {
    tab: 'FINANCIAL'
  };
  isShowRelationshipModal = false;
  coiFinancialEntityDetail: EntityDetail = new EntityDetail();
  relationValidationMap = new Map();
  $subscriptions: Subscription[] = [];
  @Output() updateRelationship: EventEmitter<any> = new EventEmitter<any>();
  @Input() isEditMode = false;
  @Input() relationshipDetails;
  @Output() positionsToView: EventEmitter<boolean> = new EventEmitter<boolean>();
  @Input() entityId: any;
  currentRelationshipDetails: any = {};
  isHoverAddRelationship = false;
  @Input() isSwitchCurrentTab = false;
  isConcurrency = false;
  hasPermissionToView = true;

  constructor(private _commonService: CommonService, private _router: Router,
    public entityDetailsServices: EntityDetailsService,
    private _activatedRoute: ActivatedRoute
  ) { }

  ngOnInit() {
    this.openRelationshipQuestionnaire();
  }

  ngOnChanges() {
    this.configuration.enableViewMode = !this.isEditMode;
    this.getDefinedRelationships();
  }

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  async getRelationshipLookUp(): Promise<any> {
    try {
      const response = await this.entityDetailsServices.addSFILookUp();
      return response;
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
        (this.isEditMode && this.entityDetailsServices.definedRelationships.length > 0) ? this.positionsToView.emit(true) : this.positionsToView.emit(false);
  }

    getQuestionnaire(data: any) {
        if (data) {
            this.entityDetailsServices.activeRelationship = data.validPersonEntityRelType.validPersonEntityRelTypeCode;
            this.entityDetailsServices.clickedTab = 'QUESTIONNAIRE';
            this.currentRelationshipDetails = data;
              if (this.relationshipDetails && this.relationshipDetails.personId === this._commonService.getCurrentUserDetail('personId') || this.hasRightToView(data.validPersonEntityRelType.disclosureTypeCode)) {
                this.hasPermissionToView = true;
                this.configuration.moduleItemKey = this._activatedRoute.snapshot.queryParamMap.get('personEntityId') || this.entityId;                this.configuration.moduleSubItemKey = data.validPersonEntityRelTypeCode;
                this.configuration = Object.assign({}, this.configuration);
              } else {
                  this.hasPermissionToView = false;
              }
        }
    }

    hasRightToView(disclosureTypeCode) {
        switch (disclosureTypeCode) {
            case '1':
                return this._commonService.getAvailableRight(['VIEW_FCOI_DISCLOSURE', 'MANAGE_FCOI_DISCLOSURE',
                    'VIEW_PROJECT_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE']);
            case '2':
                return this._commonService.getAvailableRight(['VIEW_OPA_DISCLOSURE', 'MANAGE_OPA_DISCLOSURE']);
            case '3':
                return this._commonService.getAvailableRight(['VIEW_TRAVEL_DISCLOSURE', 'MANAGE_TRAVEL_DISCLOSURE']);
        }
    }

  openRelationshipQuestionnaire() {
      this.$subscriptions.push(this.entityDetailsServices.$openQuestionnaire.subscribe((data: any) => {
        if (data) {
          this.entityDetailsServices.isRelationshipQuestionnaireChanged ? this.leaveCurrentRelationship(data) : this.getQuestionnaire(data);
        }
    }));
  }

    leaveCurrentRelationship(data: any) {
    this.entityDetailsServices.$emitUnsavedChangesModal.next({ details : data, isLeaveFromRelationTab : true });
  }

  questionnaireSaveAction(event) {
    this.entityDetailsServices.$saveQuestionnaireAction.next(event);
  }

  questionnaireEdit(event) {
    if(event) {
      this.entityDetailsServices.isRelationshipQuestionnaireChanged = true;
      let nameOfQuestionnaire = this.entityDetailsServices.definedRelationships.find(ele => ele.validPersonEntityRelType.validPersonEntityRelTypeCode == this.entityDetailsServices.activeRelationship);
      if(!this.entityDetailsServices.unSavedSections.some(ele => ele.includes('Relationship Questionnaire'))) {
        this.entityDetailsServices.unSavedSections.push( nameOfQuestionnaire.validPersonEntityRelType.description +' Relationship Questionnaire');
      }
    }
  }

}
