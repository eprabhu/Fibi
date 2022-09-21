import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { ActivatedRoute } from '@angular/router';
import { Subscription } from 'rxjs';
import { CommonService } from '../../../common/services/common.service';
import { getEndPointOptionsForArea, getEndPointOptionsForSubArea } from '../../../common/services/end-point.config';
import { AreaOfResearchService } from './area-of-research.service';
import { AreaOfResearch, InstituteProposal, InstProposal, ResearchType } from '../../institute-proposal-interfaces';
import { DataStoreService } from '../../services/data-store.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../../app-constants';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';

@Component({
	selector: 'app-area-of-research',
	templateUrl: './area-of-research.component.html',
	styleUrls: ['./area-of-research.component.css']
})
export class AreaOfResearchComponent implements OnInit, OnDestroy {

	@Input() isViewMode: boolean = true;
	generalDetails: InstProposal = new InstProposal();
	isAreaOfResearchWidgetOpen = true;
	$subscriptions: Subscription[] = [];
	instProposalId: any;
	areaOfResearchList: Array<AreaOfResearch> = [];
	areaOfResearch: AreaOfResearch = new AreaOfResearch();
	helpText: any = {};
	researchTypes: Array<ResearchType> = [];
	errorMap = new Map();
	areaSearchOptions: any = {};
	subAreaSearchOptions: any = {};
	deleteIndex = -1;
	isSaving = false;
	isResearchDescriptionReadMore = false;
	isMultiDisciplinaryDescriptionReadMore = false;
	constructor(public _commonService: CommonService,
		private _dataStore: DataStoreService,
		private _route: ActivatedRoute, private _areaOfResearch: AreaOfResearchService) { }

	ngOnInit() {
		this.getKeyPersonDetails();
		this.getGeneralDetails();
		this.getDataStoreEvent();
		this.researchTypeSet();
		this.setSearchOptionsForArea();
		this.getProposalIdFromUrl();
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

	researchTypeSet(): void {
		const researchType = this.researchTypes.find(type => type.isActive === true);
		if (researchType) {
			this.areaOfResearch.researchType = researchType;
			this.areaOfResearch.researchTypeCode = researchType.researchTypeCode;
		}
	}

	private getProposalIdFromUrl() {
		this.$subscriptions.push(this._route.queryParams.subscribe(params => {
			this.instProposalId = params.instituteProposalId;
			this.areaOfResearch.proposalId = Number(this.instProposalId);
		}));
	}

	getGeneralDetails() {
		const data: InstituteProposal = this._dataStore.getData(['instProposal']);
		this.generalDetails = data.instProposal;
	}

	getDataStoreEvent() {
		this.$subscriptions.push(this._dataStore.dataEvent
			.subscribe((data: any) => {
				if (data.includes('instituteProposalResearchAreas') || data.includes('instProposal')) {
					this.getKeyPersonDetails();
				}
			}));
	}

	getKeyPersonDetails(): void {
		const data: InstituteProposal = this._dataStore.getData(['instituteProposalResearchAreas', 'researchTypes', 'instProposal']);
		this.areaOfResearchList = data.instituteProposalResearchAreas;
		this.researchTypes = data.researchTypes;
		this.generalDetails = data.instProposal;
	}

	researchTypeChange() {
		this.areaOfResearch.researchType = this.researchTypes
			.find(area => area.researchTypeCode == this.areaOfResearch.researchTypeCode);
		this.setSearchOptionsForArea();
	}

	onResearchAreaSelect(area: any) {
		this.errorMap.clear();
		if (area) {
			this.areaOfResearch.researchTypeArea = area;
			this.areaOfResearch.researchTypeAreaCode = area.researchTypeAreaCode
		} else {
			this.areaOfResearch.researchTypeArea = null;
			this.areaOfResearch.researchTypeAreaCode = null;
		}
		this.setSearchOptionsForSubArea();
	}

	onResearchSubAreaSelect(subArea: any) {
		this.errorMap.clear();
		if (subArea) {
			this.areaOfResearch.researchTypeSubArea = subArea;
			this.areaOfResearch.researchTypeSubAreaCode = subArea.researchTypeSubAreaCode
		} else {
			this.areaOfResearch.researchTypeSubArea = null;
			this.areaOfResearch.researchTypeSubAreaCode = null;
		}
	}

	setSearchOptionsForArea() {
		const PARAMS = this.areaOfResearch.researchTypeCode ? { 'researchTypeCode': this.areaOfResearch.researchTypeCode } : null;
		this.areaSearchOptions = getEndPointOptionsForArea(PARAMS);
	}

	setSearchOptionsForSubArea() {
		const PARAMS = {
			'researchTypeCode': this.areaOfResearch.researchTypeCode ? this.areaOfResearch.researchTypeCode : null,
			'researchTypeAreaCode': this.areaOfResearch.researchTypeAreaCode ? this.areaOfResearch.researchTypeAreaCode : null
		};
		this.subAreaSearchOptions = getEndPointOptionsForSubArea(PARAMS);
	}

	validateAreaOfResearch(): void {
		this.errorMap.clear();
		if (!this.areaOfResearch.researchTypeAreaCode) {
			this.errorMap.set('area', '* Please add an Area')
		}
		if (this.checkDuplicateArea()) {
			this.errorMap.set('area', '* Area already Added')
		}
		if (this.errorMap.size == 0 && !this.isSaving) {
			this.saveAreaOfResearch();
		}
	}

	checkDuplicateArea(): boolean {
		return !!this.areaOfResearchList.find(A =>
			this.areaOfResearch.researchTypeCode === A.researchTypeCode &&
			this.areaOfResearch.researchTypeAreaCode === A.researchTypeAreaCode &&
			this.areaOfResearch.researchTypeSubAreaCode === A.researchTypeSubAreaCode);
	}

	saveAreaOfResearch() {
		this.isSaving = true;
		this.$subscriptions.push(this._areaOfResearch.saveOrUpdateSpecialReview(
			{ 'instituteProposalResearchArea': this.areaOfResearch })
			.subscribe((data: InstituteProposal) => {
				this.researchTypeSet();
				this.setSearchOptionsForArea();
				this.setSearchOptionsForSubArea();
				this.areaOfResearchList.push(data.instituteProposalResearchArea);
				this._dataStore.updateStoreData({ instituteProposalResearchAreas: this.areaOfResearchList });
				this.isSaving = false;
				this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Area added successfully.');
			}, err => {
				this.isSaving = false;
				this._commonService.showToast(HTTP_ERROR_STATUS, 'Adding Area failed. Please try again.');
			}));
	}

	deleteAreaOfResearch(): void {
		if (!this.isSaving) {
			const ID = this.areaOfResearchList[this.deleteIndex].researchAreaId;
			this.isSaving = true;
			this.$subscriptions.push(this._areaOfResearch.deleteSpecialReview(this.instProposalId, ID)
				.subscribe(data => {
					this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Area removed successfully.');
					this.areaOfResearchList.splice(this.deleteIndex, 1);
					this._dataStore.updateStoreData({ instituteProposalResearchAreas: this.areaOfResearchList });
					this.deleteIndex = -1;
					this.isSaving = false;
				}, err => {
					this.isSaving = false;
					this._commonService.showToast(HTTP_ERROR_STATUS, 'Removing Area failed. Please try again.');
			}));
		}
	}


	saveMoreInfo() {
		if (!this.isSaving) {
			this.isSaving = true;
			this._areaOfResearch.saveMoreInfo({
				proposalId: parseInt(this.instProposalId, 10),
				researchDescription: this.generalDetails.researchDescription,
				multiDisciplinaryDescription: this.generalDetails.multiDisciplinaryDescription
			}).subscribe((data) => {
				this._commonService.showToast(HTTP_SUCCESS_STATUS, 'More information saved successfully.');
				this._dataStore.updateStoreData({ 'instProposal': this.generalDetails});
				this.isSaving = true;
			}, err => {
				this._commonService.showToast(HTTP_ERROR_STATUS, 'Saving more information failed. Please try again.');
				this.isSaving = true;
			});
		}
	}

}
