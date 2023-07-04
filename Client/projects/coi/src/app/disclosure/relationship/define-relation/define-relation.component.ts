import { Component, ElementRef, EventEmitter, HostListener, Input, OnInit, Output, ViewChild } from '@angular/core';
import { MatSnackBar, MatSnackBarHorizontalPosition, MatSnackBarVerticalPosition } from '@angular/material/snack-bar';
import { slideHorizontal } from '../../../../../../fibi/src/app/common/utilities/animations';
import { environment } from '../../../../environments/environment';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { DataStoreService } from '../../services/data-store.service';
import { RelationshipService } from '../relationship.service';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subject, Subscription, interval, of, timer } from 'rxjs';
import { debounce } from 'rxjs/operators';

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
  @ViewChild('defineRelationOverlay', { static: true }) defineRelationOverlay: ElementRef;

  isMaximized: boolean = false;
  deployMap = environment.deployUrl;
  isProjectListVisible = true;
  selectedProject: any;
  $subscriptions: Subscription[] = [];
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
  horizontalPosition: MatSnackBarHorizontalPosition = 'start';
  verticalPosition: MatSnackBarVerticalPosition = 'bottom';
  coiData: any;
  clearIndex: any;
  imgUrl = this.deployMap + 'assets/images/close-black.svg';
  debounceTimer: any;
  $triggerEvent = new Subject();

  constructor(private _relationShipService: RelationshipService, public snackBar: MatSnackBar, private _commonService: CommonService, private _dataStore: DataStoreService) { }

  ngOnInit() {
    this.getDataFromStore();
    this.getEntityList();
    this.resizeEvent();
  }

  private getDataFromStore() {
    this.coiData = this._dataStore.getData();
    this.isEditMode = this.coiData.coiDisclosure.reviewStatusCode == '1';
  }

  showTaskNavBar() {
    document.body.classList.add('overflow-hidden');
    const slider = document.querySelector('.slider-base');
    slider.classList.add('slider-opened');
  }

  hideSfiNavBar(event) {
    this.addBodyScroll();
    let slider = document.querySelector('.slider-base');
    slider.classList.remove('slider-opened');
    setTimeout(() => {
        this.closePage.emit(false);
    }, 1000);
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
        if (!ele.disclComment.comment) {
          ele.disclComment.comment = this.coiDescription;
        }
      });
      this.saveClick();
    }

    getEntityList() {
      this.$subscriptions.push(  this._relationShipService.getEntityList(this.moduleCode, this.moduleItemId, this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureStatusCode,this.coiData.coiDisclosure.personId).subscribe((data: any) => {
        this.entityProjectDetails = data.coiDisclEntProjDetails;
        this.selectedProject = this.module;
        this.calculateSize();
        this.showTaskNavBar();
      }, err => {
        this.calculateSize();
        this.showTaskNavBar();
      }));
    }

    openClearConfirmationModal() {
      this.coiValidationMap.clear();
      if (this.coiStatusCode || this.coiDescription || this.isAnyOneEntityAnswered()) {
        document.getElementById('hidden-clear-all-button').click();
      }
    }

    isAnyOneEntityAnswered() {
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
        ele.disclComment.comment = null;
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
        this.entityProjectDetails[this.clearIndex ].personEntity = this.entityProjectDetails[this.clearIndex].personEntity;
        this.entityProjectDetails[this.clearIndex ].disclosureId = this.coiData.coiDisclosure.disclosureId;
        this.entityProjectDetails[this.clearIndex ].disclosureNumber =  this.coiData.coiDisclosure.disclosureNumber;
        this.entityProjectDetails[this.clearIndex ].moduleCode = this.selectedProject.moduleCode;
        this.entityProjectDetails[this.clearIndex ].moduleItemKey = this.selectedProject.moduleItemId;
        // this.getCommentObject(this.entityProjectDetails[this.clearIndex ].comment);
        this.entityProjectDetails[this.clearIndex].disclComment.comment = null;
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
      this.$subscriptions.push(this._relationShipService.saveEntityProjectRelation(this.entityProjectDetails, this.selectedProject.moduleCode, this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId,this.coiData.coiDisclosure.personId).subscribe((data: any) => {
        this.entityProjectDetails = data.coiDisclEntProjDetails;
        this.coiDescription = '';
        this.coiStatusCode = null;
        this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationships saved successfully.');
      }, err => {
        this.coiDescription = '';
        this.coiStatusCode = null;
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationships.');
      }));
    }

    singleSaveClick(element, index) {
        this.$subscriptions.push(this._relationShipService.singleEntityProjectRelation(element, this.selectedProject.moduleCode, this.selectedProject.moduleItemId, this.coiData.coiDisclosure.disclosureId,this.coiData.coiDisclosure.personId).subscribe((data: any) => {
          this.entityProjectDetails[index] = data.coiDisclEntProjDetail;
          this.clearIndex = null;
          this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Relationship saved successfully.');
      }, err => {
        this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relationship.');
      }));
    }
    
  /**
   * calculating the height of project details card, info card and table header card.
   * and making them sticky while scrolling.
   * But if the scrolling screen size smaller than these three card
   * heights sum(view height greater than 50% of screen), then removing sticky for details card, info card.
   */
  calculateSize() {
    const DETAILS_BOX = this.getClassDetails('.relationship-details-box');
    const INFO_BOX = this.isEditMode ? this.getClassDetails('.relationship-info') : null;
    const TABLE_HEADER_BOX = this.getClassDetails('.relationship-sfi-header');
    this.resetStyles(DETAILS_BOX, INFO_BOX, TABLE_HEADER_BOX);
    const DETAILS_BOX_HEIGHT = DETAILS_BOX.offsetHeight;
    const INFO_BOX_HEIGHT = INFO_BOX ? INFO_BOX.offsetHeight : 0;
    const TABLE_HEADER_BOX_HEIGHT = TABLE_HEADER_BOX.offsetHeight;
    const HALF_SCREEN_HEIGHT = window.innerHeight / 2;
    const TOTAL_BOX_HEIGHT = DETAILS_BOX_HEIGHT + INFO_BOX_HEIGHT + TABLE_HEADER_BOX_HEIGHT;
    if (TOTAL_BOX_HEIGHT > HALF_SCREEN_HEIGHT) {
      this.addRelativeRemoveSticky(DETAILS_BOX);
      if (INFO_BOX) {
        this.addRelativeRemoveSticky(INFO_BOX);
      }
      this.setTop(TABLE_HEADER_BOX, '0px');
    }
    if (TOTAL_BOX_HEIGHT < HALF_SCREEN_HEIGHT) {
      this.setTop(DETAILS_BOX, '0px');
      if (INFO_BOX) {
        this.setTop(INFO_BOX, DETAILS_BOX_HEIGHT + 'px');
      }
      this.setTop(TABLE_HEADER_BOX, (DETAILS_BOX_HEIGHT + INFO_BOX_HEIGHT) + 'px');
    }
  }

  addRelativeRemoveSticky(element) {
    element.classList.remove('position-sticky');
    element.classList.add('position-relative');
  }

  getClassDetails(className) {
    return document.querySelector<HTMLElement>(className);
  }

  setTop(element, top) {
    element.style.top = top;
  }

  resetStyles(detailsBox, infoBox, sfiHeaderBox) {
    detailsBox.classList.add('position-sticky');
    sfiHeaderBox.classList.add('position-sticky');
    sfiHeaderBox.style.top = '0px';
    detailsBox.style.top = '0px';
    if (infoBox) {
      infoBox.style.top = '0px';
      infoBox.classList.add('position-sticky');
    }
  }

  @HostListener('window:resize', ['$event'])
  onResize(event) {
    this.$triggerEvent.next();
  }

  resizeEvent() {
    this.$subscriptions.push(this.$triggerEvent.pipe(debounce(() => interval(300))).subscribe((data: any) => {
      this.calculateSize();
    }
    ));
  }

  addBodyScroll() {
      document.body.classList.remove('overflow-hidden');
      document.body.classList.add('overflow-auto');
  }

  ngOnDestroy() {
    this.addBodyScroll();
    subscriptionHandler(this.$subscriptions);
  }

}

