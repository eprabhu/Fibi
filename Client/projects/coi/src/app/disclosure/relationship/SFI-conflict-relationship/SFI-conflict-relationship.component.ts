import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { DataStoreService } from '../../services/data-store.service';
import { Subject, Subscription, interval } from 'rxjs';
import { RelationshipService } from '../relationship.service';
import { CommonService } from '../../../common/services/common.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { CoiService } from '../../services/coi.service';
import { debounce } from 'rxjs/operators';
import { deepCloneObject, hideModal, openModal, scrollIntoView } from 'projects/fibi/src/app/common/utilities/custom-utilities';

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
  isSaving = false;

  constructor(private _relationShipService: RelationshipService,
              private _commonService: CommonService,
              private _coiService: CoiService,
              private _dataStore: DataStoreService) { }

  ngOnInit() {
    this._relationShipService.isSliderDataUpdated = false;
    setTimeout(() => {
      if(this._coiService.focusSFIRelationId && this.entityProjectDetails.length) {
          scrollIntoView(this._coiService.focusSFIRelationId);
          const ELEMENT = document.getElementById(this._coiService.focusSFIRelationId);
          ELEMENT.classList.remove('border-bottom');
          ELEMENT.classList.remove('border-top');
          ELEMENT.classList.add('error-highlight-card');
          this._coiService.focusSFIRelationId = null;
      }
  });
    this.triggerSingleSave();
  }

  openSaveAllConfirmationModal() {
    this.coiValidationMap.clear();
    this.coiStatusCode = null;
    this.coiDescription == '';
    openModal('applyToAllConfirmationModal');
    this.changeCloseBtnZIndex('1000');
  }

  applyToAll() {
    this.coiValidationMap.clear();
    this.coiTableValidation.clear();
    if (!this.coiStatusCode || this.coiStatusCode == 'null') {
      this.coiValidationMap.set('coiStatus', 'Please select COI Status');
    }
    if (!this.coiDescription) {
      this.coiValidationMap.set('coiDescription', 'Please enter description');
    }
    if (this.coiValidationMap.size == 0) {
      this.saveClick();
    }
  }

  saveClick() {
    this.$subscriptions.push(this._relationShipService.saveEntityProjectRelation(
      this.prepareSaveObject(), this.selectedProject.moduleCode, this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.personId)
      .subscribe((data: any) => {
        this.entityProjectDetails = data.coiDisclEntProjDetails;
        hideModal('applyToAllConfirmationModal');
        this.changeCloseBtnZIndex(1500);
        this.coiDescription = '';
        this.coiStatusCode = null;
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationships saved successfully.');
        this._relationShipService.isSliderDataUpdated = true;
        this.closePage.emit();
      }, err => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationships.');
      }));
  }

  changeCloseBtnZIndex(zIndex) {
    let close = document.getElementById('slider-close-button');
    if (close) {
      close.style.zIndex = zIndex;
    }
  }

  prepareSaveObject() {
    const REQ_ARRAY = deepCloneObject(this.entityProjectDetails);
    return REQ_ARRAY.map((ele: any) => {
      ele.personEntityId = ele.personEntityId;
      ele.disclosureId = this.coiData.coiDisclosure.disclosureId;
      ele.disclosureNumber = this.coiData.coiDisclosure.disclosureNumber;
      ele.moduleCode = this.selectedProject.moduleCode;
      ele.moduleItemKey = this.selectedProject.moduleItemId;
      ele.projectConflictStatusCode = this.coiStatusCode;
      ele.disclComment.comment = this.coiDescription;
      ele.coiProjConflictStatusType = this.getStatusObject(ele.projectConflictStatusCode);
      return ele;
    });
  }

  getStatusObject(code) {
    return this.coiStatusList.find(ele => ele.projectConflictStatusCode === code);
  }

  // Clear modal values
  clearValues() {
    this.coiDescription = '';
    this.coiStatusCode = null;
    this.changeCloseBtnZIndex('1000');
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
      this._relationShipService.isSliderDataUpdated = true;
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
