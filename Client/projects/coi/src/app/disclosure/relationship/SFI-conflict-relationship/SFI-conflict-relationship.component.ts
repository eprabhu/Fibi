import {Component, ElementRef, EventEmitter, HostListener, Input, OnChanges, OnInit, Output, SimpleChanges, ViewChild} from '@angular/core';
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
export class SFIConflictRelationshipComponent implements OnInit, OnChanges {

  @Input() isEditMode: boolean;
  @Input() coiStatusCode: any;
  @Input() coiStatusList: any;
  // @Input() coiValidationMap: any;
  @Input() entityProjectDetails: Array<any> = [];
  // @Input() coiTableValidation: any;
  @Input() isSlider = false;
  @Input() coiDescription: any;
  @Input() selectedProject: any;
  @Input() coiData: any;
  @Input() clearIndex: any;
  @Input() moduleCode: any;
  @Input() isSavingRelation = false;
  @Output() isSavingRelationChange = new EventEmitter();
  $debounceEvent = new Subject<any>();
  @Output() closePage: EventEmitter<any> = new EventEmitter<any>();
  isApplyToAllModal = false;
  @ViewChild('relationshipConflict', { static: false }) tableResponsive: ElementRef;
  @ViewChild('conflictTableHeader', { static: false }) conflictTableHeader: ElementRef;


  $subscriptions: Subscription[] = [];
  coiValidationMap: Map<string, string> = new Map();
  coiTableValidation: Map<string, string> = new Map();
  textareaValue: string;
  isSaving = false;
  focusableId = '';

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
          this._coiService.addTableBorder(this.entityProjectDetails,'table-header-tr');
      }
  });
    this.triggerSingleSave();
  }

  ngOnChanges(changes: SimpleChanges) {
    if (changes.entityProjectDetails && !changes.entityProjectDetails.isFirstChange()) {
      this.coiValidationMap.clear();
      this.coiTableValidation.clear();
    }
	  if (this.entityProjectDetails.length) {
		  this.listenScreenSize();
	  }
  }

  openSaveAllConfirmationModal() {
    this.coiValidationMap.clear();
    this.coiStatusCode = null;
    this.coiDescription = '';
    openModal('applyToAllConfirmationModal');
	this.isApplyToAllModal = true
    // this.changeCloseBtnZIndex('1000');
  }

  applyToAll() {
    this.coiValidationMap.clear();
    this.coiTableValidation.clear();
    if (!this.coiStatusCode || this.coiStatusCode == 'null') {
      this.coiValidationMap.set('coiStatus', 'Please select Conflict Status');
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
        if(this._coiService.focusModuleId && this._coiService.focusModuleId == this.selectedProject.moduleItemId) {
            this.removeFocusId();
        }
        hideModal('applyToAllConfirmationModal');
        // this.changeCloseBtnZIndex(1500);
        this.coiDescription = '';
        this.coiStatusCode = null;
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationships saved successfully.');
        this._relationShipService.isSliderDataUpdated = true;
        this.closePage.emit();
      }, err => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationships. Please try again.');
      }));
  }

//   changeCloseBtnZIndex(zIndex) {
//     let close = document.getElementById('slider-close-button');
//     if (close) {
//       close.style.zIndex = zIndex;
//     }
//   }

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
	this.isApplyToAllModal = false;
    // this.changeCloseBtnZIndex('1000');
  }

  // Function for saving the single entity
  saveSingleEntity(index, test) {
    this.coiTableValidation.delete('save-status' + index );
    this.coiTableValidation.delete('save-description' + index );
    if (this.entityProjectDetails[index].disclComment.comment) {
      this.entityProjectDetails[index].disclComment.comment  = this.entityProjectDetails[index].disclComment.comment.trim();
    }
    if ([null, 'null'].includes(this.entityProjectDetails[index].projectConflictStatusCode)) {
      this.coiTableValidation.set('save-status' + index , 'Please select Conflict Status');
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
    } else {
        this.updateIsSavingRelation(false);
    }
  }

  singleSaveClick(element, index) {
    this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Saving....',1250);
      this.$subscriptions.push(this._relationShipService.singleEntityProjectRelation(element, this.selectedProject.moduleCode,
        this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId,
        this.coiData.coiDisclosure.personId).subscribe((data: any) => {
        this.entityProjectDetails[index] = data.coiDisclEntProjDetail;
        this.clearIndex = null;
        if(this._coiService.focusModuleId && this._coiService.focusSFIRelationId
            && this._coiService.focusModuleId == this.selectedProject.moduleItemId
            && this._coiService.focusSFIRelationId == data.coiDisclEntProjDetail.disclosureDetailsId) {
            this.removeFocusId();
        }
        this.coiValidationMap.clear();
        this.closePage.emit();
        this.updateIsSavingRelation(false);
        this.focusLastEditedInput();
    }, err => {
      setTimeout(() => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationship. Please try again.');
        this.updateIsSavingRelation(false);
        this.focusLastEditedInput();
      }, 1500);
    }));
}

removeFocusId() {
    const ELEMENT = document.getElementById(this._coiService.focusModuleId);
    ELEMENT.classList.remove('error-highlight-card');
	  this.removeBorder();
    this._coiService.focusModuleId = null;
    this._coiService.focusSFIRelationId = null;
}

    focusLastEditedInput() {
        setTimeout(() => {
            if (this.focusableId) {
                document?.getElementById(this.focusableId)?.focus();
                this.focusableId = '';
            }
        });
    }

sliderDataChanges() {
  this._dataStore.dataChanged = false;
  this._relationShipService.isSliderInputModified = false;
}

sfiSingleSave(index, sfi, focusableId: string) {
  this.focusableId = focusableId;
  this.updateIsSavingRelation(true);
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

isShowWarning(data) {
  return data.prePersonEntityId && data.prePersonEntityId != data.personEntityId;
}

    updateIsSavingRelation(value: boolean) {
        this.isSavingRelation = value;
        this.isSavingRelationChange.emit(this.isSavingRelation);
    }

	@HostListener('document:keydown.escape', ['$event'])
	handleEscapeEvent(event: any): void {
		if ((event.key === 'Escape' || event.key === 'Esc') && this.isApplyToAllModal) {
			document.getElementById('claim-sumbit-no-btn').click();
			this.clearValues();
		}
	}

  @HostListener('window:resize', ['$event'])
  listenScreenSize() {
    if (this.isSlider) {
      const INFO_CARD_HEIGHT = document.getElementById('info-card')?.offsetHeight ||0 ;
      const HEADER_HEIGHT = document.getElementById('relationship-details-body')?.offsetHeight ||0;
      const Slider_height = this.conflictTableHeader.nativeElement.offsetHeight;

      if (window.innerWidth >= 1200) {
        this.tableResponsive.nativeElement.style.maxHeight = (window.innerHeight - (INFO_CARD_HEIGHT + HEADER_HEIGHT + Slider_height + 60)) + 'px';
      } else {
        const HEADER_HEIGHT = document.getElementById('relationship-details-slider-header')?.offsetHeight;
        this.tableResponsive.nativeElement.style.maxHeight = (window.innerHeight - (HEADER_HEIGHT + 18)) + 'px';
      }

    }
  }

  private removeBorder() {
    if (this._coiService.focusSFIRelationId) {
      const INDEX = this.entityProjectDetails.findIndex(ele => ele.disclosureDetailsId == this._coiService.focusSFIRelationId);
      if (INDEX != -1) {
        if (INDEX == 0) {
          const ELEMENT = document.getElementById('table-header-tr');
          ELEMENT.classList.remove('border-bottom-0');
        } else {
          const ELEMENT_ID = (this.entityProjectDetails[INDEX - 1].disclosureDetailsId).toString();
          const ELEMENT = document.getElementById(ELEMENT_ID);
          ELEMENT.classList.remove('border-bottom-0');
        }
      }
    }
  }
}
