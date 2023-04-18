import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../../common/services/common.service';
import { CoiService } from '../../../services/coi.service';
import { DataStoreService } from '../../../services/data-store.service';
import { EntityRelationshipService } from '../entity-relationship.service';
import {subscriptionHandler} from "../../../../../../../fibi/src/app/common/utilities/subscription-handler";
import {HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS} from "../../../../../../../fibi/src/app/app-constants";

class ProjectDetails {
  projectNumber = '';
  projectName = '';
}
@Component({
  selector: 'app-entity-list',
  templateUrl: './entity-list.component.html',
  styleUrls: ['./entity-list.component.css']
})
export class EntityListComponent implements OnInit {

  @Output() openModal = new EventEmitter<any>();

  isProjectListVisible = true;
  coiStatusList: any = [];
  selectedProject: any;
  $subscriptions: Subscription[] = [];
  coiStatusCode: any;
  coiDescription: any;
  sfiDetails: any;
  dependencies = ['coiDisclosure'];
  currentProjectId: any;
	isOpenModal = false;

  newArray: Array<any> = [];
  disclosureNumber: any;
  disclosureStatusCode: any;
  isEditMode: boolean;
  personId: any;
  isShowInfo = true;
  coiDisclosure: any;
  constructor(public _entityService: EntityRelationshipService,
        private _dataStore: DataStoreService,
        public _coiService: CoiService,
        private _commonService: CommonService,
    private _route: ActivatedRoute) { }

  ngOnInit() {
    this._coiService.isShowInfo = true;
    this.getDataFromStore();
    this.listenDataChangeFromStore();
    !this._entityService.linkedProposalId && this.coiDisclosure.coiDisclosureCategoryType.disclosureCategoryTypeCode != 3
    ? this.getProposalRecord()
    // tslint:disable-next-line: radix
    : this.getEntityProjectRelations(3, parseInt(this._entityService.linkedProposalId), true);
    this.getCoiStatusList();
  }

  toggleToolkitVisibility(): void {
		this.isProjectListVisible = !this.isProjectListVisible;
		this._entityService.$isProjectListVisible.next(this.isProjectListVisible);
	}

  getProposalRecord() {
    this.$subscriptions.push(this._entityService.$setSelectedProjectDetails.subscribe((data: any) => {
      this.selectedProject = data;
      this._entityService.currentCommonProjectArray =
			this._entityService.commonProjectArray.findIndex((ele) => ele.moduleItemId == data.moduleItemId);
      this.getEntityProjectRelations(this.selectedProject.moduleCode,
        this.selectedProject.moduleItemId, false);
    }));
  }

  getEntityProjectRelations(moduleCode, moduleItemId, isProposalDisclosure) {
    this.$subscriptions.push(
      this._entityService.getEntityProjectRelations(moduleCode, moduleItemId,
        this._route.snapshot.queryParamMap.get('disclosureId'),
        this.disclosureStatusCode,
        this.personId, isProposalDisclosure).subscribe((data: any) => {
        this.coiDescription = '';
        this.coiStatusCode = null;
        if (data && data.coiDisclosureDetails.length > 0) {
        this.newArray = data.coiDisclosureDetails;
        if (this._entityService.linkedProposalId && this.coiDisclosure.coiDisclosureCategoryType.disclosureCategoryTypeCode == 3) {
          this.selectedProject = data.proposals[0];
        }
      }
    }));
  }

  setPreviousNext(type) {
    const CURRENT = this._entityService.commonProjectArray[this._entityService.currentCommonProjectArray];
    if (!CURRENT.sfiCompleted && this.isEditMode) {
      this._entityService.nextProjectPosition = type;
    if (type === 'P' && this._entityService.currentCommonProjectArray != 0) {
      const PREVIOUS = this._entityService.commonProjectArray[this._entityService.currentCommonProjectArray - 1];
      this._entityService.currentProject.projectNumber = CURRENT.moduleCode === 3 ? CURRENT.moduleItemId : CURRENT.moduleItemKey;
      this._entityService.currentProject.projectName = CURRENT.title;
      this._entityService.currentProjectModuleItemKey = CURRENT.moduleItemId;
      this._entityService.nextProject.projectNumber = PREVIOUS.moduleCode === 3 ? PREVIOUS.moduleItemId : PREVIOUS.moduleItemKey;
      this._entityService.nextProject.projectName = PREVIOUS.title;
      this._entityService.nextProjectModuleItemKey = PREVIOUS.moduleItemId;
      this.openModal.emit(true);
    } else if (type === 'N' && this._entityService.currentCommonProjectArray !== this._entityService.commonProjectArray.length - 1) {
      const NEXT = this._entityService.commonProjectArray[this._entityService.currentCommonProjectArray + 1];
      this._entityService.currentProject.projectNumber = CURRENT.moduleCode === 3 ? CURRENT.moduleItemId : CURRENT.moduleItemKey;
      this._entityService.currentProject.projectName = CURRENT.title;
      this._entityService.currentProjectModuleItemKey = CURRENT.moduleItemId;
      this._entityService.nextProject.projectNumber = NEXT.moduleCode === 3 ? NEXT.moduleItemId : NEXT.moduleItemKey;
      this._entityService.nextProject.projectName = NEXT.title;
      this._entityService.nextProjectModuleItemKey = NEXT.moduleItemId;
      this.openModal.emit(true);
    } else {
      // tslint:disable-next-line: triple-equals
      this._entityService.currentCommonProjectArray != 0 ?
      this._commonService.showToast(HTTP_SUCCESS_STATUS, 'No more next project available') :
      this._commonService.showToast(HTTP_SUCCESS_STATUS, 'No more previous project available');
    }
    } else {
      this._entityService.$previousNext.next(type);
    }
  }

  getCoiStatusList() {
    this.$subscriptions.push(this._entityService.$coiStatus.subscribe((data: any) => {
      this.coiStatusList = data;
    }));
  }

saveClick() {
  this.prepareSaveObject();
  this.$subscriptions.push(
    this._entityService.saveEntityProjectRelation(this.newArray, this.selectedProject.moduleCode, this.selectedProject.moduleItemId,this._route.snapshot.queryParamMap.get('disclosureId'), this.personId).subscribe((data: any) => {
      this._entityService.$updateSFIComplete.next(data.sfiCompleted);
      if (!this._entityService.linkedProposalId && this.coiDisclosure.coiDisclosureCategoryType.disclosureCategoryTypeCode != 3 ) {
        this.setPreviousNext('N');
      }
  }, err => {
    this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in saving relations');
  }));
}

prepareSaveObject() {
  this.newArray.forEach((ele: any) => {
    ele.coiFinancialEntityId = ele.coiFinancialEntity.coiFinancialEntityId;
    ele.disclosureId = this._route.snapshot.queryParamMap.get('disclosureId');
    ele.disclosureNumber = this.disclosureNumber;
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
    comment.disclosureNumber = this.disclosureNumber;
    comment.commentTypeCode = 1;
    comment.comments = comment.comments ? comment.comments : null;
}

applyToAll() {
  this.newArray.forEach((ele: any) => {
    if (!ele.discDetStatusCode) {
      ele.discDetStatusCode = this.coiStatusCode;
    } if (!ele.comment.comments) {
      ele.comment.comments = this.coiDescription;
    }
  });
}

clearAll() {
  this.newArray.forEach((ele: any) => {
    ele.discDetStatusCode = null;
    ele.comment.comments = null;
  });
  this.coiStatusCode = null;
  this.coiDescription = '';
}

private getDataFromStore() {
  const DATA = this._dataStore.getData(this.dependencies);
  this.coiDisclosure = DATA.coiDisclosure;
  this.disclosureNumber = DATA.coiDisclosure.disclosureNumber;
  this.disclosureStatusCode = DATA.coiDisclosure.disclosureStatusCode;
  this.personId = DATA.coiDisclosure.personId;
  this.isEditMode = this._dataStore.getEditModeForCOI();
}

private listenDataChangeFromStore() {
  this.$subscriptions.push(
      this._dataStore.dataEvent.subscribe((dependencies: string[]) => {
          if (dependencies.some((dep) => this.dependencies.includes(dep))) {
              this.getDataFromStore();
          }
      })
  );
}

  ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

  clearDisclosureInfo() {
    this._coiService.isShowInfo = false;
  }

}
