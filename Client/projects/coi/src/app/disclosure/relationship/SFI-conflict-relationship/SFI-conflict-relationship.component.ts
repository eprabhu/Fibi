import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { DataStoreService } from '../../services/data-store.service';
import { Subscription } from 'rxjs';
import { RelationshipService } from '../relationship.service';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { CoiService } from '../../services/coi.service';

@Component({
  selector: 'app-SFI-conflict-relationship',
  templateUrl: './SFI-conflict-relationship.component.html',
  styleUrls: ['./SFI-conflict-relationship.component.scss']
})
export class SFIConflictRelationshipComponent implements OnInit {

  @Input() isEditMode: boolean;
  @Input() coiStatusCode: any;
  @Input() coiStatusList: any;
  // @Input() coiValidationMap: any;
  @Input() entityProjectDetails: Array<any> = [];
  // @Input() coiTableValidation: any;
  @Input() coiDescription: any;
  @Input() selectedProject: any;
  @Input() coiData: any;
  @Input() clearIndex: any;
  @Input() moduleCode: any;
  @Output() closePage: EventEmitter<any> = new EventEmitter<any>();


  $subscriptions: Subscription[] = [];
  coiValidationMap: Map<string, string> = new Map();
  coiTableValidation: Map<string, string> = new Map();

  constructor(private _relationShipService: RelationshipService,
              private _commonService: CommonService,
              private _coiService: CoiService,
              private _dataStore: DataStoreService) { }

  ngOnInit() {
  }

  openSaveAllConfirmationModal() {
    this.coiValidationMap.clear();
    if (this.coiStatusCode && this.coiDescription) {
      document.getElementById('hidden-save-all-button').click();
    }
    if (!this.coiStatusCode) {
      this.coiValidationMap.set('coiStatus', 'Please select COI Status');
    }
    if (!this.coiDescription) {
      this.coiValidationMap.set('coiDescription', 'Please enter description');
    }
  }

  setUnsavedModuleTrue() {
    this._coiService.unSavedModules = 'Relationships';
    this._dataStore.dataChanged = true;
  }

  applyToAll() {
    this.coiValidationMap.clear();
    this.coiTableValidation.clear();
    this.entityProjectDetails.forEach((ele: any) => {
        ele.projectConflictStatusCode = this.coiStatusCode;
        ele.disclComment.comment = this.coiDescription;
    });
    this.saveClick();
  }

  saveClick() {
    this.prepareSaveObject();
    this.$subscriptions.push(this._relationShipService.saveEntityProjectRelation(
      this.entityProjectDetails, this.selectedProject.moduleCode, this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.personId).subscribe((data: any) => {
      this.entityProjectDetails = data.coiDisclEntProjDetails;
      this.coiDescription = '';
      this.coiStatusCode = null;
      this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationships saved successfully.');
      this.closePage.emit();
    }, err => {
      this.coiDescription = '';
      this.coiStatusCode = null;
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationships.');
    }));
  }

  prepareSaveObject() {
    this.entityProjectDetails.forEach((ele: any) => {
      ele.personEntityId = ele.personEntityId;
      ele.disclosureId = this.coiData.coiDisclosure.disclosureId;
      ele.disclosureNumber =  this.coiData.coiDisclosure.disclosureNumber;
      ele.moduleCode = this.selectedProject.moduleCode;
      ele.moduleItemKey = this.selectedProject.moduleItemId;
      ele.coiProjConflictStatusType = this.getStatusObject(ele.projectConflictStatusCode);
    });
  }

  getStatusObject(code) {
    return this.coiStatusList.find(ele => ele.projectConflictStatusCode === code);
  }

  // Clear modal values
  clearValues() {
    this.coiDescription = '';
    this.coiStatusCode = null;
  }

  // Function for saving the single entity
  saveSingleEntity(index, test) {
    this.coiTableValidation.delete('save-status' + index );
    this.coiTableValidation.delete('save-description' + index );
    if ([null, 'null'].includes(this.entityProjectDetails[index].projectConflictStatusCode)) {
      this.coiTableValidation.set('save-status' + index , 'Please select COI Status');
    }
    if (!this.entityProjectDetails[index].disclComment.comment) {
      this.coiTableValidation.set('save-description' + index , 'Please enter description');
    }
    if (!this.coiTableValidation.has('save-status' + index) && !this.coiTableValidation.has('save-description' + index) ) {
      test.personEntityId = test.personEntityId;
      test.disclosureId = this.coiData.coiDisclosure.disclosureId;
      test.disclosureNumber =  this.coiData.coiDisclosure.disclosureNumber;
      test.moduleCode = this.selectedProject.moduleCode;
      test.moduleItemKey = this.selectedProject.moduleItemId;
      // this.getCommentObject(test.comment);
      test.coiProjConflictStatusType = this.getStatusObject(test.projectConflictStatusCode);
      this.singleSaveClick(test, index);
    }
  }

  singleSaveClick(element, index) {
    this.$subscriptions.push(this._relationShipService.singleEntityProjectRelation(element, this.selectedProject.moduleCode,
      this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId,
      this.coiData.coiDisclosure.personId).subscribe((data: any) => {
      this.entityProjectDetails[index] = data.coiDisclEntProjDetail;
      this.clearIndex = null;
      this.coiValidationMap.clear();
      this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationship saved successfully.');
      this.closePage.emit();
  }, err => {
    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationship.');
  }));
}
}
