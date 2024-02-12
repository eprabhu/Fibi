import { Component, ElementRef, EventEmitter, HostListener, Input, OnInit, Output, ViewChild } from '@angular/core';
import { MatSnackBar, MatSnackBarHorizontalPosition, MatSnackBarVerticalPosition } from '@angular/material/snack-bar';
import { slideHorizontal } from '../../../../../../fibi/src/app/common/utilities/animations';
import { environment } from '../../../../environments/environment';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { DataStoreService } from '../../services/data-store.service';
import { RelationshipService } from '../relationship.service';
import { subscriptionHandler } from '../../../../../../fibi/src/app/common/utilities/subscription-handler';
import { Subject, Subscription, interval } from 'rxjs';
import { debounce } from 'rxjs/operators';
import { CoiService } from '../../services/coi.service';
import { scrollIntoView } from '../../../../../../fibi/src/app/common/utilities/custom-utilities';
import { openCoiSlider } from '../../../common/utilities/custom-utilities';

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
  @Input() relationList: any;
  @Input() currentRelation: any;
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
  validStatusCodes = ['1', '5', '6'];
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
  isDataModified = false;
  isShowSlider = false;

  constructor(private _relationShipService: RelationshipService, private _coiService: CoiService, public snackBar: MatSnackBar, private _commonService: CommonService, private _dataStore: DataStoreService) { }

  ngOnInit() {
    this.getDataFromStore();
    this.getEntityList();
    this.resizeEvent();
  }

  private getDataFromStore() {
    this.coiData = this._dataStore.getData();
    this.isEditMode = this.validStatusCodes.includes(this.coiData.coiDisclosure.reviewStatusCode);
  }

  showTaskNavBar() {
    if (!this.isShowSlider) {
      this.isShowSlider = true;
      openCoiSlider('relationship-details');
    }
  }

    openSnackBar(message: string, action: string) {
      this.snackBar.open(message, action, {
        duration:5000,
          verticalPosition: 'top',
          horizontalPosition: 'right',
      });
   }

    getEntityList() {
      this.$subscriptions.push(  this._relationShipService.getEntityList(this.moduleCode, this.moduleItemId, this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureStatusCode,this.coiData.coiDisclosure.personId).subscribe((data: any) => {
        this.entityProjectDetails = data;
        this.selectedProject = this.module;
        this.calculateSize();
        this.showTaskNavBar();
        setTimeout(() => {
          if(this._coiService.focusSFIRelationId) {
              scrollIntoView(this._coiService.focusSFIRelationId);
              const ELEMENT = document.getElementById(this._coiService.focusSFIRelationId);
              ELEMENT.classList.add('error-highlight-card');
              this._coiService.focusSFIRelationId = null;
          }
        });
      }, err => {
        this.calculateSize();
        this.showTaskNavBar();
      }));
    }

    isAnyOneEntityAnswered() {
       return !!this.entityProjectDetails.find(ele => ele.projectConflictStatusCode);
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

    // getCommentObject(comment: any) {
    //     comment.disclosureNumber = this.coiData.disclosureNumber;
    //     comment.commentTypeCode = 1;
    //     comment.comments = comment.comments ? comment.comments : null;
    // }

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
          this.coiValidationMap.clear();
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
    const TABLE_HEADER_BOX = this.getClassDetails('.relationship-sfi-header') || null;
    const GLOBAL_SAVE = this.getClassDetails('.relationship-sfi-global-save');
    this.resetStyles(DETAILS_BOX, INFO_BOX, GLOBAL_SAVE, TABLE_HEADER_BOX);
    const DETAILS_BOX_HEIGHT = DETAILS_BOX.offsetHeight;
    const INFO_BOX_HEIGHT = INFO_BOX ? INFO_BOX.offsetHeight : 0;
    const TABLE_HEADER_BOX_HEIGHT = TABLE_HEADER_BOX ? TABLE_HEADER_BOX.offsetHeight: 0;
    const GLOBAL_SAVE_HEIGHT = GLOBAL_SAVE.offsetHeight;
    const HALF_SCREEN_HEIGHT = window.innerHeight / 2;
    const TOTAL_BOX_HEIGHT = DETAILS_BOX_HEIGHT + INFO_BOX_HEIGHT + TABLE_HEADER_BOX_HEIGHT + GLOBAL_SAVE_HEIGHT;
    if (TOTAL_BOX_HEIGHT > HALF_SCREEN_HEIGHT) {
      this.addRelativeRemoveSticky(DETAILS_BOX);
      if (INFO_BOX) {
        this.addRelativeRemoveSticky(INFO_BOX);
      }
      this.addRelativeRemoveSticky(GLOBAL_SAVE);
      this.setTop(TABLE_HEADER_BOX, '0px');
    }
    if (TOTAL_BOX_HEIGHT < HALF_SCREEN_HEIGHT) {
      this.setTop(DETAILS_BOX, '0px');
      if (INFO_BOX) {
        this.setTop(INFO_BOX, DETAILS_BOX_HEIGHT + 'px');
      }
      this.setTop(GLOBAL_SAVE, DETAILS_BOX_HEIGHT + INFO_BOX_HEIGHT +'px');
      this.setTop(TABLE_HEADER_BOX , (DETAILS_BOX_HEIGHT + INFO_BOX_HEIGHT + GLOBAL_SAVE_HEIGHT) + 'px');
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
    if(element) {
      element.style.top = top;
    }
  }

  resetStyles(detailsBox, infoBox, globalSave, sfiHeaderBox) {
    detailsBox.classList.add('position-sticky');
    detailsBox.style.top = '0px';
    if(sfiHeaderBox) {
      sfiHeaderBox.classList.add('position-sticky');
      sfiHeaderBox.style.top = '0px';
    }
    if (infoBox) {
      infoBox.style.top = '0px';
      infoBox.classList.add('position-sticky');
    }
    globalSave.classList.add('position-sticky');
    globalSave.style.top = '0px';
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

  ngOnDestroy() {
    subscriptionHandler(this.$subscriptions);
  }

  getColorBadges(typeCode) {
    switch (typeCode) {
        case 3:
            return 'bg-proposal-clip';
        case 1:
            return 'bg-award-clip';
        default:
            return;
    }
  }

  hideConflictNavBar() {
    setTimeout(() => {
        this.isShowSlider = false;
        this.closePage.emit();
    }, 500);
    this._dataStore.dataChanged = false;
    this._relationShipService.isSliderInputModified = false;
}

validateProjectSfiSliderOnClose() {
    this.hideConflictNavBar();
}

  goBackStep() {
    this.currentRelation--;
    this.navigateToStep(this.relationList[this.currentRelation]);
  }

  navigateToStep(relation) {
    this.moduleCode = relation.moduleCode;
    this.moduleItemId = relation.moduleItemId;
    this.module = relation;
    this.getEntityList();
  }

  goToStep(stepPosition?: any) {
    this.currentRelation = stepPosition ? stepPosition : this.currentRelation + 1;
    this.navigateToStep(this.relationList[this.currentRelation]);
  }

}

