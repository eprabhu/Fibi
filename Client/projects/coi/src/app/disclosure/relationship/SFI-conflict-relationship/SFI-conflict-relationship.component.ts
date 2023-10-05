import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { DataStoreService } from '../../services/data-store.service';
import { Subject, Subscription, interval } from 'rxjs';
import { RelationshipService } from '../relationship.service';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { CoiService } from '../../services/coi.service';
import { debounce } from 'rxjs/operators';

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
  $debounceEvent = new Subject<any>();
  @Output() closePage: EventEmitter<any> = new EventEmitter<any>();


  $subscriptions: Subscription[] = [];
  coiValidationMap: Map<string, string> = new Map();
  coiTableValidation: Map<string, string> = new Map();
  textareaValue: string;

  constructor(private _relationShipService: RelationshipService,
              private _commonService: CommonService,
              private _coiService: CoiService,
              private _dataStore: DataStoreService) { }

  ngOnInit() {
    this._relationShipService.isSliderDataUpdated = false;
    this.triggerSingleSave();
  }

  openSaveAllConfirmationModal() {
    this.coiValidationMap.clear();
    if (this.coiStatusCode && this.coiDescription) {
      document.getElementById('hidden-save-all-button').click();
      this.sliderDataChanges();
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
    this._relationShipService.isSliderInputModified = true;
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
      this.entityProjectDetails, this.selectedProject.moduleCode, this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.personId)
      .subscribe((data: any) => {
        this.entityProjectDetails = data.coiDisclEntProjDetails;
        this.coiDescription = '';
        this.coiStatusCode = null;
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationships saved successfully.');
        this._relationShipService.isSliderDataUpdated = true;
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
      this.sliderDataChanges();
    }
  }

  singleSaveClick(element, index) {
    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Saving....',1250);
      this.$subscriptions.push(this._relationShipService.singleEntityProjectRelation(element, this.selectedProject.moduleCode,
        this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId,
        this.coiData.coiDisclosure.personId).subscribe((data: any) => {
        this.entityProjectDetails[index] = data.coiDisclEntProjDetail;
        this.clearIndex = null;
        this.coiValidationMap.clear();
        setTimeout(() => {
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationship saved successfully.');
        }, 1500);
        this.closePage.emit();
    }, err => {
      setTimeout(() => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationship.');
      }, 1500);
    }));
}

sliderDataChanges() {
  this._dataStore.dataChanged = false;
  this._relationShipService.isSliderInputModified = false;
}

sfiSingleSave(index, sfi) {
  this.$debounceEvent.next({index: index, SFI: sfi});
}

triggerSingleSave() {
  this.$subscriptions.push(this.$debounceEvent.pipe(debounce(() => interval(1000))).subscribe((data: any) => {
    if (data) {
      this.saveSingleEntity(data.index, data.SFI);
    }
  }
  ));
}
getStatusDescriptionByCode(code: string): string {
  const STATUS = this.coiStatusList.find(S => S.projectConflictStatusCode === code);
  return STATUS ? STATUS.description : '';
}


}
