import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { BehaviorSubject } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { EntityDetailsService } from '../entity-details.service';

@Component({
  selector: 'app-entity-questionnaire',
  templateUrl: './entity-questionnaire.component.html',
  styleUrls: ['./entity-questionnaire.component.scss']
})
export class EntityQuestionnaireComponent implements OnInit {

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

  constructor(private _commonService: CommonService, private _router: Router, private _entityDetailsServices: EntityDetailsService) { }

  ngOnInit() {
    this.getDataFromService();
    this.configuration.enableViewMode = true;
  }

  getDataFromService() {
    this.getDefinedRelationships()
    this.getQuestionnaire(this.definedRelationships[0])
  }
  getSaveEvent(_event) {
    this.relationLookup.length ? this.addRelations() : this.navigateBack();
    this.$externalSaveEvent.next(true);
  }
  addRelations(flag = false) {
    this.isAddRelationButtonToggled = flag;
  }

  navigateBack() {
    this._router.navigateByUrl(this._entityDetailsServices.previousURL);
  }
  getDefinedRelationships() {
    this._entityDetailsServices.$entityDetails.subscribe((res: any) => {
      this.configuration.moduleItemKey = res.coiFinancialEntity.coiFinancialEntityId;
      this.definedRelationships = res.coiFinancialEntityDetails;
    });
  }

  getQuestionnaire(data: any) {
    this.activeRelationship = data.financialEntityRelTypeCode;
    this.configuration.moduleSubItemKey = data.financialEntityRelTypeCode;
    this.configuration = Object.assign({}, this.configuration);
  }
}
