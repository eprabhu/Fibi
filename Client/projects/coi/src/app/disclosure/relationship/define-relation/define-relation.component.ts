import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { MatSnackBar, MatSnackBarHorizontalPosition, MatSnackBarVerticalPosition } from '@angular/material/snack-bar';
import { slideHorizontal } from '../../../../../../fibi/src/app/common/utilities/animations';
import { environment } from '../../../../environments/environment';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { DataStoreService } from '../../services/data-store.service';
import { RelationshipService } from '../relationship.service';
declare var $: any;

@Component({
  selector: 'app-define-relation',
  templateUrl: './define-relation.component.html',
  styleUrls: ['./define-relation.component.scss'],
  animations: [slideHorizontal]
})
export class DefineRelationComponent implements OnInit {

  @Input() coiStatusList = [];
  @Input() moduleItemId: any;
  @Input() moduleCode: any;
  @Input() module: any;
  @Output() closePage: EventEmitter<any> = new EventEmitter<any>();

  isMaximized: boolean = false;
  deployMap = environment.deployUrl;
  isProjectListVisible = true;
  selectedProject: any;
  // $subscriptions: Subscription[] = [];
  coiStatusCode: any = null;
  coiDescription: any;
  reviewStatusCode: any;
  sfiDetails: any;
  dependencies = ['coiDisclosure'];
  currentProjectId: any;
	isOpenModal = false;
  coiValidationMap: Map<string, string> = new Map();
  coiTableValidation: Map<string, string> = new Map();

  entityProjectDetails: Array<any> = [];
  disclosureNumber: any;
  disclosureStatusCode: any;
  isEditMode: boolean = true;
  personId: any;
  isShowInfo = true;
  coiDisclosure: any;
  isShowDetails = false;
  horizontalPosition: MatSnackBarHorizontalPosition = 'start';
  verticalPosition: MatSnackBarVerticalPosition = 'bottom';
  coiData: any;
  clearIndex: any;

  @ViewChild('relationShipOverlay', { static: true }) relationShipOverlay: ElementRef;

  constructor(private _relationShipService: RelationshipService, public snackBar: MatSnackBar, private _commonService: CommonService, private _dataStore: DataStoreService) { }

  ngOnInit() {
    this.getDataFromStore();
    this.getEntityList();
  }

  private getDataFromStore() {
    this.coiData = this._dataStore.getData();
    this.isEditMode = this.coiData.coiDisclosure.reviewStatusCode == '1';
  }

  showTaskNavBar() {
    if (this.isShowDetails) {
      this.relationShipOverlay.nativeElement.style.display = 'block';
      document.documentElement.classList.add('cdk-global-scrollblock');
      setTimeout(() => {
        document.getElementById("close-define-relation").focus();
      })
    } else {
      this.relationShipOverlay.nativeElement.style.display = 'none';
      document.documentElement.classList.remove('cdk-global-scrollblock');
    }
  }

  hideSfiNavBar() {
        this.relationShipOverlay.nativeElement.style.display = 'block';
        document.documentElement.classList.remove('cdk-global-scrollblock');
        this.isShowDetails = false;
        setTimeout(() => {
          this.closePage.emit(false);
        }, 1000)
    }

    openSnackBar(message: string, action: string) {
      this.snackBar.open(message, action, {
        duration:5000,
          verticalPosition: 'top',
          horizontalPosition: 'right',
      });
   }

    applyToAll() {
      this.coiValidationMap.clear();
      this.coiTableValidation.clear();
      this.entityProjectDetails.forEach((ele: any) => {
        if (!ele.projectConflictStatusCode) {
          ele.projectConflictStatusCode = this.coiStatusCode;
        }
        // if (!ele.comment.comments) {
        //   ele.comment.comments = this.coiDescription;
        // }
      });
      this.saveClick();
    }

    getEntityList() {
      this._relationShipService.getEntityList(this.moduleCode, this.moduleItemId, this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureStatusCode).subscribe((data: any) => {
        this.entityProjectDetails = data.coiDisclEntProjDetails;
        this.selectedProject = this.module;
        this.isShowDetails = true;
        this.showTaskNavBar();
      }, err => {
        this.isShowDetails = true;
        this.showTaskNavBar();
      });
    }

    openClearConfirmationModal() {
      this.coiValidationMap.clear();
      if (this.coiStatusCode || this.coiDescription || this.isAnyOneEntityAnswered()) {
        document.getElementById('hidden-clear-all-button').click();
      }
    }

    isAnyOneEntityAnswered() {
      // || ele.comment.comments
       return !!this.entityProjectDetails.find(ele => ele.projectConflictStatusCode);
    }

    openSaveAllConfirmationModal() {
      this.coiValidationMap.clear();
      if (this.coiStatusCode) {
        document.getElementById('hidden-save-all-button').click();
      } else {
        this.coiValidationMap.set('coiStatus', '* Please select COI Status');
      }
    }

    clearAll() {
      this.entityProjectDetails.forEach((ele: any) => {
        ele.projectConflictStatusCode = null;
        // ele.comment.comments = null;
      });
      this.coiStatusCode = null;
      this.coiDescription = '';
      this.coiTableValidation.clear();
      this.saveClick();
    }

    clearSingleEntity() {
      this.coiTableValidation.delete('save'+this.clearIndex );
      // if (!this.newArray[index].projectConflictStatusCode && !this.newArray[index].comment.comments) {
      //   this.coiTableValidation.set('clear'+index , 'No data to clear');
      // } else {
        this.entityProjectDetails[this.clearIndex ].projectConflictStatusCode = null;
        // this.entityProjectDetails[this.clearIndex ].comment.comments = null;
        this.entityProjectDetails[this.clearIndex ].personEntity = this.entityProjectDetails[this.clearIndex ].personEntity.personEntity;
        this.entityProjectDetails[this.clearIndex ].disclosureId = this.coiData.coiDisclosure.disclosureId;
        this.entityProjectDetails[this.clearIndex ].disclosureNumber =  this.coiData.coiDisclosure.disclosureNumber;
        this.entityProjectDetails[this.clearIndex ].moduleCode = this.selectedProject.moduleCode;
        this.entityProjectDetails[this.clearIndex ].moduleItemKey = this.selectedProject.moduleItemId;
        // this.getCommentObject(this.entityProjectDetails[this.clearIndex ].comment);
        this.entityProjectDetails[this.clearIndex ].coiProjConflictStatusType = this.getStatusObject(this.entityProjectDetails[this.clearIndex ].projectConflictStatusCode);
        this.singleSaveClick(this.entityProjectDetails[this.clearIndex], this.clearIndex);
    }

    openClearModal(index) {
      this.coiTableValidation.clear();
      this.clearIndex = index;
      // || this.entityProjectDetails[index].comment.comments)
      if (this.entityProjectDetails[index].projectConflictStatusCode) {
        document.getElementById('hidden-single-clear-button').click();
      }
    }

    saveSingleEntity(index, test) {
      this.coiTableValidation.delete('save'+index );
      if ([null, 'null'].includes(this.entityProjectDetails[index].projectConflictStatusCode)) {
        this.coiTableValidation.set('save'+index , 'Please select COI Status');
      } else {
        test.personEntityId = test.personEntity.personEntityId;
        test.disclosureId = this.coiData.coiDisclosure.disclosureId;
        test.disclosureNumber =  this.coiData.coiDisclosure.disclosureNumber;
        test.moduleCode = this.selectedProject.moduleCode;
        test.moduleItemKey = this.selectedProject.moduleItemId;
        // this.getCommentObject(test.comment);
        test.coiProjConflictStatusType = this.getStatusObject(test.projectConflictStatusCode);
        this.singleSaveClick(test, index);
      }
    }

    prepareSaveObject() {
      this.entityProjectDetails.forEach((ele: any) => {
        ele.personEntityId = ele.personEntity.personEntityId;
        ele.disclosureId = this.coiData.coiDisclosure.disclosureId;
        ele.disclosureNumber =  this.coiData.coiDisclosure.disclosureNumber;
        ele.moduleCode = this.selectedProject.moduleCode;
        ele.moduleItemKey = this.selectedProject.moduleItemId;
        // this.getCommentObject(ele.comment);
        ele.coiProjConflictStatusType = this.getStatusObject(ele.projectConflictStatusCode);
      });
    }

    getStatusObject(code) {
      return this.coiStatusList.find(ele => ele.projectConflictStatusCode === code);
    }

    getCommentObject(comment: any) {
        comment.disclosureNumber = this.coiData.disclosureNumber;
        comment.commentTypeCode = 1;
        comment.comments = comment.comments ? comment.comments : null;
    }

    saveClick() {
      this.prepareSaveObject();
        this._relationShipService.saveEntityProjectRelation(this.entityProjectDetails, this.selectedProject.moduleCode, this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId).subscribe((data: any) => {
          this.entityProjectDetails = data.coiDisclEntProjDetails;
        }, err => {
      });
    }

    singleSaveClick(element, index) {
        this._relationShipService.singleEntityProjectRelation(element, this.selectedProject.moduleCode, this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId).subscribe((data: any) => {
          this.entityProjectDetails[index] = data.coiDisclEntProjDetail;
          this.clearIndex = null;
          this._commonService.showToast(HTTP_ERROR_STATUS, 'Relationship saved successfully.');
      }, err => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving.');
      });
    }

}

