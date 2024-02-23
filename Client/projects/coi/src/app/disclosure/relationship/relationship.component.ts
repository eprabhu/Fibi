import { Component, OnInit } from '@angular/core';
import {  Router} from '@angular/router';
import { slideHorizontal } from '../../../../../fibi/src/app/common/utilities/animations';
import { environment } from '../../../environments/environment';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { DataStoreService } from '../services/data-store.service';
import { RelationshipService } from './relationship.service';
import { Subscription } from 'rxjs';
import { RO } from '../coi-interface';
import { subscriptionHandler } from 'projects/fibi/src/app/common/utilities/subscription-handler';
import { CoiService } from '../services/coi.service';
import { scrollIntoView } from '../../../../../fibi/src/app/common/utilities/custom-utilities';

@Component({
  selector: 'app-relationship',
  templateUrl: './relationship.component.html',
  styleUrls: ['./relationship.component.scss'],
  animations: [slideHorizontal],
})
export class RelationshipComponent implements OnInit {
  isShowRelation = false;
  proposalArray = [];
  coiStatusList = [];
  deployMap = environment.deployUrl;
  searchText: any;
  searchResult = [];
  moduleId: any;
  moduleCode: any;
  selectedProject: any;
  coiData: any;
  isShowWhiteIcon = false;
  selectedProjectForView: any;
  isEditMode: boolean = true;
  collapseViewMore = {};
  expandInfo = false;
  count: number;
  disclosureId: number;
  reviewStatus: string;
  personId: string;
  filterType = 'ACTIVE';
  currentPage = 1;
  $subscriptions: Subscription[] = [];
  dependencies = ['coiDisclosure', 'numberOfSFI'];
  isShowNoDataCard = false;
  awardList = [];
  isShowCollapsedConflictRelationship = false;
  entityProjectDetails: Array<any> = [];
  coiValidationMap: Map<string, string> = new Map();
  coiTableValidation: Map<string, string> = new Map();
  coiStatusCode: any = null;
  currentRelation: number;
  switchRelationView = false;

  constructor(private _relationShipService: RelationshipService,
              private _dataStore: DataStoreService,
              public _router: Router,
              private _commonService: CommonService,
              public coiService: CoiService
              ) { }

  closePage(event: any) {
    this.isShowRelation = false;
    this.moduleCode = null;
    this.moduleId = null;
    if (this._relationShipService.isSliderDataUpdated) {
      this.updateConflictStatus();
    }
    if (this.isShowCollapsedConflictRelationship) {
      this.updateRelationStatus();
    } else {
      this.loadProjectRelations();
    }
  }

  ngOnInit() {
    this.getDataFromStore();
    this.loadProjectRelations(true);
    this.getDependencyDetails();
  }

getDisclosureCount(typeCode, disclosureStatus) {
    if (disclosureStatus) {
      const VALUE = disclosureStatus.find(ele => Object.keys(ele) == typeCode);
      return VALUE ? VALUE[typeCode] : 0;
    }
  }

  private updateConflictStatus(): void {
    this.$subscriptions.push( this._relationShipService.updateConflictStatus(this.coiData.coiDisclosure.disclosureId).subscribe((data: any) => {
      if (data) {
        this.coiData.coiDisclosure.coiConflictStatusType = data;
        this.coiData.coiDisclosure.conflictStatusCode = data.conflictStatusCode;
        this._dataStore.updateStore(['coiDisclosure'],  this.coiData);
        this._relationShipService.isSliderDataUpdated = false;
      }
    }, err => {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating status');
    }));
  }

  private getDataFromStore() {
    this.coiData = this._dataStore.getData();
    const IS_CREATE_USER = this.coiData.coiDisclosure.personId === this._commonService.getCurrentUserDetail('personId');
    this.isEditMode = ['1', '5', '6'].includes(this.coiData.coiDisclosure.reviewStatusCode) && IS_CREATE_USER;
  }

  loadProjectRelations(isFristTimeLoad = false) {
    if (isFristTimeLoad) {
      this.isShowNoDataCard = false;
    }
    this.$subscriptions.push(this._relationShipService.getProjectRelations(this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureStatusCode).subscribe((data: any) => {
      if (data) {
        if (isFristTimeLoad) {
          this.isShowNoDataCard = true;
          this.awardList = data.awards;
          data.proposals.every(ele => this.awardList.push(ele));
        } else {
          const combinedArray = [...data.awards, ...data.proposals];
          if (combinedArray.length === this.awardList.length) {
            combinedArray.forEach((project, index) => {
              if (this.awardList[index].moduleItemId === project.moduleItemId) {
                this.awardList[index].disclosureStatusCount = project.disclosureStatusCount;
                this.awardList[index].sfiCompleted = project.sfiCompleted;
              }
            });
          } else {
            this.awardList = combinedArray;
          }
        }
        this.coiStatusList = data.coiProjConflictStatusTypes;
        if (this.awardList.length === 1) {
          this.isShowCollapsedConflictRelationship = true;
          this.getEntityList();
        }
        if(!this.isShowCollapsedConflictRelationship) {
          setTimeout(() => {
            if(this.coiService.focusModuleId) {
                scrollIntoView(this.coiService.focusModuleId);
                const ELEMENT = document.getElementById(this.coiService.focusModuleId);
                ELEMENT.classList.add('error-highlight-card');
                this.coiService.focusModuleId = null;
            }
          },100);
        }
      }
    }));
  }

  updateRelationStatus() {
    this.$subscriptions.push(this._relationShipService.getProjectRelations(this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureStatusCode).subscribe((data: any) => {
      if (data) {
        const LINKED_MODULE = data?.awards[0] || data?.proposals[0];
        this.awardList[0].disclosureStatusCount = LINKED_MODULE.disclosureStatusCount;
        this.awardList[0].sfiCompleted = LINKED_MODULE.sfiCompleted;
      }
    }));
  }

  /**
   * No Conflict - 1
   * Potential Conflict - 2
   * conflict - 3
   */
  getDisclosureStatus(): any {
   let test : any;
    this.awardList.forEach(ele => {
         test = ele.disclosureStatusCount.find(ele => ele[3] > 0) ? '3' :
                ele.disclosureStatusCount.find(ele => ele[2] > 0) ? '2' :
                ele.disclosureStatusCount.find(ele => ele[1] > 0) ? '1' : null;
    });
    return test;
  }

  openDefineRelationship(test, currentRelation) {
    this.moduleCode = test.moduleCode;
    this.moduleId = test.moduleItemId ;
    this.selectedProject = test;
    this.currentRelation = currentRelation;
    this.isShowRelation = true;
  }

  collapseViewMoreOption(id: number, flag: boolean): void {
    this.collapseViewMore[id] = !flag;
  }

  openProjectMoreDetails(moduleId, moduleCode) {
    if (moduleCode == 3) {
      const redirectToProposal = this._commonService.fibiApplicationUrl + '#/fibi/proposal/overview?proposalId=' + moduleId;
      window.open (redirectToProposal);
      // this._router.navigate([test,{ queryParams: { proposalId:  moduleId}}]);
    } else {
      const redirectToAward = this._commonService.fibiApplicationUrl + '#/fibi/award/overview?awardId=' + moduleId;
      window.open (redirectToAward);
      // this._router.navigate([test2,{ queryParams: { awardId:  moduleId}}]);
    }
  }

getRequestObject() {
  const REQ_OBJ = new RO();
  REQ_OBJ.currentPage = 0;
  REQ_OBJ.disclosureId = this.disclosureId;
  REQ_OBJ.filterType = this.filterType;
  REQ_OBJ.pageNumber = 0;
  REQ_OBJ.personId = this.personId;
  REQ_OBJ.reviewStatusCode = this.reviewStatus;
  REQ_OBJ.searchWord = '';
  return REQ_OBJ;
}

getDependencyDetails() {
    const DATA = this._dataStore.getData(this.dependencies);
    this.reviewStatus = DATA && DATA.coiDisclosure ? DATA.coiDisclosure.reviewStatusCode : '';
    this.disclosureId =  DATA && DATA.coiDisclosure ? DATA.coiDisclosure.disclosureId : null;
    this.personId = DATA && DATA.coiDisclosure ? DATA.coiDisclosure.personId : '';
  }

  getEntityList() {
    this.$subscriptions.push(this._relationShipService.getEntityList(
      this.moduleCode, this.moduleId, this.coiData.coiDisclosure.disclosureId,
      this.coiData.coiDisclosure.disclosureStatusCode, this.coiData.coiDisclosure.personId).subscribe((data: any) => {
      this.entityProjectDetails = data;
    }, err => {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Something Went wrong!');
    }));
  }

  ngOnDestroy(): void {
    subscriptionHandler(this.$subscriptions);
}

    switchRelationViewMode(newValue) {
      if (this.switchRelationView !== newValue && !newValue) {
          this.ngOnInit();
      }
      this.switchRelationView = newValue;
    }

}
