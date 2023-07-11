import { Component, OnInit } from '@angular/core';
import {  Router} from '@angular/router';
import { slideHorizontal } from '../../../../../fibi/src/app/common/utilities/animations';
import { environment } from '../../../environments/environment';
import { HTTP_ERROR_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { DataStoreService } from '../services/data-store.service';
import { RelationshipService } from './relationship.service';
import { SfiService } from '../sfi/sfi.service';
import { Subscription } from 'rxjs';
import { GetSFIRequestObject } from '../coi-interface';

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
  expandInfo = true;
  count: number;
  disclosureId: number;
  reviewStatus: string;
  personId: string;
  filterType = 'ACTIVE';
  currentPage = 1;
  $subscriptions: Subscription[] = [];
  dependencies = ['coiDisclosure', 'numberOfSFI'];
  isShowNoDataCard = false;

  constructor(private _relationShipService: RelationshipService,
              private _dataStore: DataStoreService,
              public _router: Router,
              private _commonService: CommonService,
              private _sfiService: SfiService) { }

  closePage() {
    this.isShowRelation = false;
    this.moduleCode = null;
    this.moduleId = null;
    if(this.isEditMode) {
      this.updateConflictStatus();
      this.loadProjectRelations(); 
    }
  }

  ngOnInit() {
    this.getDataFromStore();
    this.loadProjectRelations();
    this.getDependencyDetails();
    this.getSfiDetails();
  }

  getDisclosureCount(typeCode, disclosureStatus) {
    if(disclosureStatus) {
      let value = disclosureStatus.find(ele => Object.keys(ele) == typeCode);
      return value ? value[typeCode] : 0;
    }
  }

  private updateConflictStatus(): void {
    this._relationShipService.updateConflictStatus(this.coiData.coiDisclosure.disclosureId).subscribe((data: any) => {
      if(data) {
        this.coiData.coiDisclosure.coiConflictStatusType = data;
        this.coiData.coiDisclosure.conflictStatusCode = data.conflictStatusCode;
        this._dataStore.updateStore(['coiDisclosure'],  this.coiData);
      }
    }, err => {
      this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in updating status');
    });
  }

  private getDataFromStore() {
    this.coiData = this._dataStore.getData();
    this.isEditMode = this.coiData.coiDisclosure.reviewStatusCode == '1';
  }

  loadProjectRelations() {
    this.isShowNoDataCard = false;
    this._relationShipService.getProjectRelations(this.coiData.coiDisclosure.disclosureId, this.coiData.coiDisclosure.disclosureStatusCode).subscribe((data: any) => {
      if (data) {
        this.isShowNoDataCard = true;
        this.proposalArray = data.awards;
        data.proposals.every(ele => this.proposalArray.push(ele));
        this.coiStatusList = data.coiProjConflictStatusTypes;
      }
    });
  }

  /**
   * No Conflict - 1
   * Potential Conflict - 2
   * conflict - 3
   */
  getDisclosureStatus(): any {
   let test : any;
    this.proposalArray.forEach(ele => {
         test = ele.disclosureStatusCount.find(ele => ele[3] > 0) ? '3' :
                ele.disclosureStatusCount.find(ele => ele[2] > 0) ? '2' :
                ele.disclosureStatusCount.find(ele => ele[1] > 0) ? '1' : null;
    });
    return test;
  }

  openDefineRelationship(test) {
    this.moduleCode = test.moduleCode;
    this.moduleId = test.moduleItemId ;
    this.selectedProject = test;
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

  getSfiDetails() {
    this.$subscriptions.push(this._sfiService.getSfiDetails(this.getRequestObject()).subscribe((data: any) => {
        if (data) {
            this.count = data.count;
        }
    }));
}

getRequestObject() {
  const REQ_OBJ = new GetSFIRequestObject();
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
}
