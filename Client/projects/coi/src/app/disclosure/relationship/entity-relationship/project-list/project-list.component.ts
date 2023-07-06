import { Component, EventEmitter, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../../common/services/common.service';
import { DataStoreService } from '../../../services/data-store.service';
import { EntityRelationshipService } from '../entity-relationship.service';
import {slideHorizontal} from "../../../../../../../fibi/src/app/common/utilities/animations";
import {scrollIntoView} from "../../../../../../../fibi/src/app/common/utilities/custom-utilities";
import {subscriptionHandler} from "../../../../../../../fibi/src/app/common/utilities/subscription-handler";
import { GetSFIRequestObject } from '../../../coi-interface';


@Component({
	selector: 'app-project-list',
	templateUrl: './project-list.component.html',
	styleUrls: ['./project-list.component.css'],
	animations: [slideHorizontal]
})
export class ProjectListComponent implements OnInit {

	@Output() openModal = new EventEmitter<any>();
	currentProjectTab = 'PROJECT';
	isProjectListVisible = true;
	$subscriptions: Subscription[] = [];
	proposalArray: any = [];
	awardArray: any = [];
	sfiArray: any = [];
	commonArray: any;
	currentProjectId: any;
	isOpenModal = false;
	isAwardCollapse = true;
	isProposalCollapse = true;
	isSFICollapse = true;
	isEditMode: boolean;
	disclosureId: any;
	disclosureStatusCode: any;

	dependencies = ['coiDisclosure'];
	personId: any;

	constructor(public _entityService: EntityRelationshipService,
        private _dataStore: DataStoreService,
		private _commonService: CommonService) { }

	ngOnInit() {
		this.getProjectListVisibility();
		this.getProjectDetails();
		this.traverseList();
		this.updateSFIStatus();
		this.getDataFromStore();
    	this.listenDataChangeFromStore();
	}

	getProjectDetails() {
		this.$subscriptions.push(this._entityService.projectDetails.subscribe((data: any) => {
			this.proposalArray = data.proposals;
			this.awardArray = data.awards;
			this.getCommonArray();
		}));
	}

	getCommonArray() {
		this.$subscriptions.push(this._entityService.test.subscribe((data: any) => {
			this.commonArray = data;
			if (this.proposalArray.length > 0) {
				this.setSelectedProject(this.proposalArray[0]);
			} else if (this.awardArray.length > 0) {
				this.setSelectedProject(this.awardArray[0]);
			}
		}));
	}


	private getProjectListVisibility(): void {
		this.$subscriptions.push(this._entityService.$isProjectListVisible.subscribe((data: boolean) => {
			(this.isProjectListVisible = data) ? this.collapseToolKit() : this.expandToolKit();
		}));
	}

	expandToolKit(): void {
		(document.getElementById('project-list') as HTMLElement).style.width = '100%';
	}

	collapseToolKit(): void {
		(document.getElementById('project-list') as HTMLElement).style.width = '75%';
	}

	setSelectedProject(proposal, index = null) {
		if (proposal) {
		this.currentProjectId = proposal.moduleItemId;
		if (index != null) {
			this.setPreviousNext('O', proposal);
		} else {
			this._entityService.$setSelectedProjectDetails.next(proposal);
			this._entityService.currentCommonProjectArray =
			this.commonArray.findIndex((ele) => ele.moduleItemId == proposal.moduleItemId);
		}
		}
	}

	traverseList() {
		this.$subscriptions.push(this._entityService.$previousNext.subscribe((data) => {
			// tslint:disable-next-line: triple-equals
			if (data === 'P' && this._entityService.currentCommonProjectArray != 0) {
				let a = this._entityService.currentCommonProjectArray;
				this.setSelectedProject(this._entityService.commonProjectArray
				[a - 1]);
				scrollIntoView(this._entityService.commonProjectArray
					[a - 1].moduleItemId);
			} else if (data === 'N' && this._entityService.currentCommonProjectArray !== this._entityService.commonProjectArray.length - 1) {
				this.setSelectedProject(this._entityService.commonProjectArray
				[this._entityService.currentCommonProjectArray + 1]);
				scrollIntoView(this._entityService.commonProjectArray
					[this._entityService.currentCommonProjectArray - 1].moduleItemId);
			} else {
				// tslint:disable-next-line: triple-equals
				// this._entityService.currentCommonProjectArray != 0 ?
					// this._commonService.showToast(HTTP_SUCCESS_STATUS, 'No more next project available') :
					// this._commonService.showToast(HTTP_SUCCESS_STATUS, 'No more previous project available');
			}
		}));
	}

	scrollToSection(id: string) {
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

	setPreviousNext(type, NEXT) {
		const CURRENT = this._entityService.commonProjectArray[this._entityService.currentCommonProjectArray];
		this._entityService.currentProject.projectNumber = CURRENT.moduleCode === 3 ? CURRENT.moduleItemId : CURRENT.moduleItemKey;
		this._entityService.currentProject.projectName = CURRENT.title;
		this._entityService.currentProjectModuleItemKey = CURRENT.moduleItemId;
		if (!CURRENT.sfiCompleted && this.isEditMode) {
		   this._entityService.nextProjectPosition = type;
		  this._entityService.nextProject.projectNumber = NEXT.moduleCode === 3 ? NEXT.moduleItemId : NEXT.moduleItemKey;
		  this._entityService.nextProject.projectName = NEXT.title;
		  this._entityService.nextProjectModuleItemKey = NEXT.moduleItemId;
		  this.openModal.emit(true);
		} else {
			this._entityService.$setSelectedProjectDetails.next(NEXT);
			this._entityService.currentCommonProjectArray =
			this.commonArray.findIndex((ele) => ele.moduleItemId == NEXT.moduleItemId);
		}
}

	updateSFIStatus() {
			this.$subscriptions.push(this._entityService.$updateSFIComplete.subscribe((data) => {
				this._entityService.commonProjectArray[this._entityService.currentCommonProjectArray].sfiCompleted = data;
			}));
	}

	getSFIDetails() {
		this.$subscriptions.push(this._entityService.getSfiDetails(this.getRequestObject()).subscribe((data: any) => {
			this.sfiArray = data;
			this.currentProjectTab = "SFI";
		}));
	}

	getRequestObject() {
		const REQ_OBJ = new GetSFIRequestObject();
        REQ_OBJ.currentPage = 0;
        REQ_OBJ.disclosureId = this.disclosureId;
        REQ_OBJ.filterType = '';
        REQ_OBJ.pageNumber = 0;
        REQ_OBJ.personId = this.personId;
        REQ_OBJ.reviewStatusCode = '';
        REQ_OBJ.searchWord = '';
        return REQ_OBJ;
    }

	private getDataFromStore() {
		const DATA = this._dataStore.getData(this.dependencies);
		this.disclosureStatusCode = DATA.coiDisclosure.disclosureStatusCode;
		this.disclosureId =  DATA.coiDisclosure.disclosureId;
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
}
