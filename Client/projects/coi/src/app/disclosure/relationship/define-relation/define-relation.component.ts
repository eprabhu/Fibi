import { Component, ElementRef, EventEmitter, Input, OnInit, Output, ViewChild } from '@angular/core';
import { MatSnackBar, MatSnackBarHorizontalPosition, MatSnackBarVerticalPosition } from '@angular/material/snack-bar';
import { Toast } from 'bootstrap';
import { slideHorizontal } from '../../../../../../fibi/src/app/common/utilities/animations';
import { environment } from '../../../../environments/environment';
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
  sfiDetails: any;
  dependencies = ['coiDisclosure'];
  currentProjectId: any;
	isOpenModal = false;
  coiValidationMap: Map<string, string> = new Map();
  coiTableValidation: Map<string, string> = new Map();

  newArray: Array<any> = [];
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
  toast:any;
  clearIndex: any;

  @ViewChild('relationShipOverlay', { static: true }) relationShipOverlay: ElementRef;
  @ViewChild('myToast',{static:true}) toastEl: ElementRef

  constructor(private _relationShipService: RelationshipService, public snackBar: MatSnackBar, private _dataStore: DataStoreService) { }

  ngOnInit() {
    this.toast=new Toast(this.toastEl.nativeElement,{})
    this.getDataFromStore();
    this.getEntityList();
  }

  private getDataFromStore() {
    this.coiData = this._dataStore.getData();
    console.log(this.coiData)
  }

  showTaskNavBar() {
    if (this.isShowDetails) {
      this.relationShipOverlay.nativeElement.style.display = 'block';
      document.documentElement.classList.add('cdk-global-scrollblock');
    } else {
      this.relationShipOverlay.nativeElement.style.display = 'none';
      document.documentElement.classList.remove('cdk-global-scrollblock');
    }
  }

  hideSfiNavBar() {
        this.relationShipOverlay.nativeElement.style.display = 'block';
        document.documentElement.classList.add('cdk-global-scrollblock');
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
      this.newArray.forEach((ele: any) => {
        if (!ele.discDetStatusCode) {
          ele.discDetStatusCode = this.coiStatusCode;
        } if (!ele.comment.comments) {
          ele.comment.comments = this.coiDescription;
        }
      });
      this.saveClick();
    }

    getEntityList() {
      this._relationShipService.getEntityList(this.moduleCode, this.moduleItemId, this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureStatusCode).subscribe((data: any) => {
        this.newArray = data.coiDisclosureDetails;
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
       return !!this.newArray.find(ele => ele.discDetStatusCode || ele.comment.comments);
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
      this.newArray.forEach((ele: any) => {
        ele.discDetStatusCode = null;
        ele.comment.comments = null;
      });
      this.coiStatusCode = null;
      this.coiDescription = '';
      this.saveClick();
    }

    clearSingleEntity() {
      this.coiTableValidation.delete('save'+this.clearIndex );
      // if (!this.newArray[index].discDetStatusCode && !this.newArray[index].comment.comments) {
      //   this.coiTableValidation.set('clear'+index , 'No data to clear');
      // } else {
        this.newArray[this.clearIndex ].discDetStatusCode = null;
        this.newArray[this.clearIndex ].comment.comments = null;
        this.newArray[this.clearIndex ].coiFinancialEntityId = this.newArray[this.clearIndex ].coiFinancialEntity.coiFinancialEntityId;
        this.newArray[this.clearIndex ].disclosureId = this.coiData.coiDisclosure.disclosureId;
        this.newArray[this.clearIndex ].disclosureNumber =  this.coiData.coiDisclosure.disclosureNumber;
        this.newArray[this.clearIndex ].moduleCode = this.selectedProject.moduleCode;
        this.newArray[this.clearIndex ].moduleItemKey = this.selectedProject.moduleItemId;
        this.getCommentObject(this.newArray[this.clearIndex ].comment);
        this.newArray[this.clearIndex ].coiDisclosureDetailsStatus = this.getStatusObject(this.newArray[this.clearIndex ].discDetStatusCode);
        this.singleSaveClick(this.newArray[this.clearIndex], this.clearIndex);
    }

    openClearModal(index) {
      this.coiTableValidation.clear();
      this.clearIndex = index;
      if (this.newArray[index].discDetStatusCode || this.newArray[index].comment.comments) {
        document.getElementById('hidden-single-clear-button').click();
      }
    }

    saveSingleEntity(index, test) {
      this.coiTableValidation.delete('save'+index );
      if ([null, 'null'].includes(this.newArray[index].discDetStatusCode)) {
        this.coiTableValidation.set('save'+index , 'Please select COI Status');
      } else {
        test.coiFinancialEntityId = test.coiFinancialEntity.coiFinancialEntityId;
        test.disclosureId = this.coiData.coiDisclosure.disclosureId;
        test.disclosureNumber =  this.coiData.coiDisclosure.disclosureNumber;
        test.moduleCode = this.selectedProject.moduleCode;
        test.moduleItemKey = this.selectedProject.moduleItemId;
        this.getCommentObject(test.comment);
        test.coiDisclosureDetailsStatus = this.getStatusObject(test.discDetStatusCode);
        this.singleSaveClick(test, index);
      }
    }

    prepareSaveObject() {
      this.newArray.forEach((ele: any) => {
        ele.coiFinancialEntityId = ele.coiFinancialEntity.coiFinancialEntityId;
        ele.disclosureId = this.coiData.coiDisclosure.disclosureId;
        ele.disclosureNumber =  this.coiData.coiDisclosure.disclosureNumber;
        ele.moduleCode = this.selectedProject.moduleCode;
        ele.moduleItemKey = this.selectedProject.moduleItemId;
        this.getCommentObject(ele.comment);
        ele.coiDisclosureDetailsStatus = this.getStatusObject(ele.discDetStatusCode);
      });
    }

    getStatusObject(code) {
      return this.coiStatusList.find(ele => ele.discDetStatusCode === code);
    }

    getCommentObject(comment: any) {
        comment.disclosureNumber = '1000-0037';
        comment.commentTypeCode = 1;
        comment.comments = comment.comments ? comment.comments : null;
    }

    saveClick() {
      this.prepareSaveObject();
        this._relationShipService.saveEntityProjectRelation(this.newArray, this.selectedProject.moduleCode, this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId).subscribe((data: any) => {
          this.newArray = data.coiDisclosureDetails;
        }, err => {
      });
    }

    singleSaveClick(element, index) {
        this._relationShipService.singleEntityProjectRelation(element, this.selectedProject.moduleCode, this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId).subscribe((data: any) => {
          this.newArray[index] = data.coiDisclosureDetail;
          this.toast.show();
          this.clearIndex = null;
      }, err => {
      });
    }

}

