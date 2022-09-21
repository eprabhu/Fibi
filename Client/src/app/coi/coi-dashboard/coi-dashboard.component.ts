import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
import { ElasticConfigService } from '../../common/services/elastic-config.service';
import { getEndPointOptionsForDepartment, getEndPointOptionsForProposalDisclosure, getEndPointOptionsForSponsor } from '../../common/services/end-point.config';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { DataStoreService } from '../services/data-store.service';
import { CoiDashboardRequestObject, Disclosure, SFIDashboardRequestObject } from './coi-dashboard';
import { CoiDashboardService } from './coi-dashboard.service';

declare var $: any;
@Component({
	selector: 'app-coi-dashboard',
	templateUrl: './coi-dashboard.component.html',
	styleUrls: ['./coi-dashboard.component.css']
})
export class CoiDashboardComponent implements OnInit, OnDestroy {

	disclosureArray: Array<any> = [];
	tempDisclosureArray: Array<any> = [];
	isShowCountModal = false;
	selectedModuleCode: any;
	historyArray: any[] = [];
	dashboardRequestObject: CoiDashboardRequestObject;
	$subscriptions: Subscription[] = [];
	isActiveDisclosureAvailable: boolean;
	isShowActiveWarning: boolean;
	disclosureIdForView: any;
	currentDisclosureId: any;
	currentDisclosureNumber: any;
	reviseDisclosureObject: any = {};
	reviseObject: any = {reviseComment: null, disclosureId: null};
	disclosureNumber: any;
	isActiveFlag = true;
	sfiDashboardRequestObject: SFIDashboardRequestObject;
	inputType: any;
	currentSFI: any;
	disclosureSequenceStatusCode: any;
	searchName: any;
	isShowSearch = false;
	searchId: any;
	personId: any;
	isShowDisclosureInfo = true;
	isShowTabInfo = true;
	clearLUField: String;
	unitHttpOptions: any = {};
	proposalDisclosure: any = [];
	clearProposalField: String;
	proposalHttpOptions: any = {};
	proposalList: any = [];
	ProposalSearchOptions: any;
	proposalDetails: any = {};
	isSearchExternalProposal = false;
	piElasticSearchOptions: any = {};
	clearPIField: String;
	sponsorSearchOptions: any = {};
	clearFieldSponsorField: String;
	isHover = false;
	proposalId: any;
	isShowResultCard = false;
	clearProposalData: any;
	historyTabArray = [
		{reviseComment: 'Test', eventName: 'Annual', dispositionStatus: 'Approved', reviewStatus: 'Complete', createDate: '02/08/2022', certificationDate: '02/08/2022', updateUserFullName: 'Smith, Will', updateTimestamp : '02/08/2022 4:36:34 PM'},
		{reviseComment: '', eventName: 'Revision', dispositionStatus: 'Pending', reviewStatus: 'Pending', createDate: '02/08/2022', certificationDate: '02/08/2022', updateUserFullName: 'Smith, Will', updateTimestamp : '02/08/2022 4:36:34 PM'},
		{reviseComment: 'Revise comment', eventName: 'Proposal', dispositionStatus: 'Pending', reviewStatus: 'Pending', createDate: '02/08/2022', certificationDate: '02/08/2022', updateUserFullName: 'Smith, Will', updateTimestamp : '02/08/2022 4:36:34 PM'},
		{reviseComment: 'Revise comment', eventName: 'Travel', dispositionStatus: 'Pending', reviewStatus: 'Pending', createDate: '02/08/2022', certificationDate: '02/08/2022', updateUserFullName: 'Smith, Will', updateTimestamp : '02/08/2022 4:36:34 PM'},
	];
	disPersonDetails: any;
    projectDisclosureValidation = new Map();

	constructor(private _coiDashboardService: CoiDashboardService,
		public _dataStore: DataStoreService,
		public _commonService: CommonService,
		private _router: Router,
		private _elasticConfig: ElasticConfigService) { }

	ngOnInit() {
		this.dashboardRequestObject = new CoiDashboardRequestObject();
		this.sfiDashboardRequestObject = new SFIDashboardRequestObject();
		this.fetchDashboardData();
		this.ProposalSearchOptions = getEndPointOptionsForProposalDisclosure();
		this.unitHttpOptions = getEndPointOptionsForDepartment();
		this.piElasticSearchOptions = this._elasticConfig.getElasticForPerson();
		this.sponsorSearchOptions = getEndPointOptionsForSponsor();
	}

	getButtonLabel() {
		switch (this._dataStore.currentDashboardTab) {
			case 'CURRENT_DISCLOSURES': {
				return 'Create New FCOI Disclosure';
			}
			case 'PROPOSAL_DISCLOSURES': {
				return 'Create Proposal Disclosure';
			}
			case 'TRAVEL_DISCLOSURES': {
				return 'Create New Travel Disclosure ';
			}
		}
	}

	closeModal(event) {
		this.isShowCountModal = event;
	}

	setSelectedModuleCode(moduleName, id, coiNumber, statusCode, type, pId) {
		switch (moduleName) {
			case 'sfi': {this.selectedModuleCode = 8;
				break;
			}
			case 'award': {
				this.selectedModuleCode = 1;
				break;
			}
			case 'proposal': {
				this.selectedModuleCode = 3;
				break;
			}
			case 'disclosure': {
				this.selectedModuleCode = 101;
				break;
			}
			default: this.selectedModuleCode = 0;
		}
		this.isShowCountModal = true;
		this.inputType = type;
		this.currentDisclosureId = id;
		this.currentDisclosureNumber = coiNumber;
		this.disclosureSequenceStatusCode = statusCode;
		this.personId = pId;
	}

	openHistoryModal( disclosureNumber) {
		this.historyArray = [];
		this.$subscriptions.push(this._coiDashboardService.getCOIHistory(disclosureNumber).subscribe((data: any) => {
			this.historyArray = data ? data.coiDisclosures : [];
			this.disPersonDetails = data.person;
			if (this.historyArray.length > 0) {
				this.historyArray.map(a => {
					if (a.disclosureCategoryTypeCode == 1) {
						a.eventName = a.disclosureSequenceStatusCode == 1 && this.isActiveDisclosureAvailable ? 'Revision' : 'Annual'
					} else {
						a.eventName = a.disclosureCategoryTypeCode == 3 ? 'Proposal' : 'Travel';
					}
				});
			}
		}));
		$('#historyModal').modal('show');
	}

	public fetchDashboardData(): void {
		this.dashboardRequestObject.tabName = this._dataStore.currentDashboardTab;
        this.dashboardRequestObject.pageNumber = this._dataStore.currentDashboardTab === 'CURRENT_DISCLOSURES' ? 2 : 30;
		if (this._dataStore.currentDashboardTab !== 'SIGNIFICANT_FINANCIAL_INTERESTS') {
			this.$subscriptions.push(this._coiDashboardService.getCOIDashboard(this.dashboardRequestObject).subscribe((data: any) => {
				this.disclosureArray = data.disclosureViews ? data.disclosureViews : [];
                this.isShowTabInfo = this.disclosureArray.length ? false : true;
                this.disclosureArray.map (ele => {
					ele.numberOfProposals = ele.disclosureStatusCode != 1 ? ele.noOfProposalInActive : ele.noOfProposalInPending;
					ele.numberOfAwards = ele.disclosureStatusCode != 1 ? ele.noOfAwardInActive : ele.noOfAwardInPending;
				});
				this.setEventTypeFlag();
			}));
		} else {
			this.$subscriptions.push(this._coiDashboardService.getSFIDashboard(this.sfiDashboardRequestObject).subscribe((data: any) => {
				this.disclosureArray = data.coiFinancialEntityList;
                this.isShowTabInfo = this.isShowTabInfo ? true : this.disclosureArray.length ? false : true;
			}));
		}
	}

	setEventTypeFlag() {
		this.isActiveDisclosureAvailable = !!this.disclosureArray.find((ele: Disclosure) => ele.disclosureSequenceStatusCode == 2);
	}

	viewSFIs() {
	}

	setReviewMessageFlag(disclosure) {
		if (disclosure.disclosureSequenceStatusCode == 1 && this.isActiveDisclosureAvailable) {
			this.isShowActiveWarning = false;
			this.getViewId();
			$('#reviewModal').modal('show');
		} else if (disclosure.disclosureSequenceStatusCode == 2) {
			if (!!this.disclosureArray.find((ele: Disclosure) => ele.disclosureSequenceStatusCode == 1)) {
				this.isShowActiveWarning = true;
				this.getViewId();
				$('#reviewModal').modal('show');
			} else {
				this.getDisclosureNumber();
				this.reviseObject.disclosureId = disclosure.coiDisclosureId;
				$('#reviewConfirmationModal').modal('show');
			}
		}
	}

	getViewId() {
		this.disclosureIdForView = this.disclosureArray.find((ele: Disclosure) => ele.disclosureSequenceStatusCode == 1).coiDisclosureId;
	}

	reviseDisclosure() {
		this.$subscriptions.push(this._coiDashboardService.reviseDisclosure(this.reviseObject)
			.subscribe(data => {
				this.reviseDisclosureObject = data;
				this.clearModal();
				this._commonService.showToast(HTTP_SUCCESS_STATUS, 'New version of disclosure created.');
				this._router.navigate(['/fibi/coi/screening-questionnaire'], {
					queryParams: {
						disclosureId: this.reviseDisclosureObject.coiDisclosure.disclosureId
					}
				});
			},
				err => {
					this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in creating new version. Please try again.');
				}));
	}

	getDisclosureNumber() {
		this.disclosureNumber = this.disclosureArray.find((ele: Disclosure) => ele.disclosureSequenceStatusCode == 2).coiDisclosureNumber;
	}

	clearModal(): void {
		$('#reviewConfirmationModal').modal('hide');
		this.reviseObject.reviseComment = null;
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

	noDataText() {
		switch (this._dataStore.currentDashboardTab) {
			case 'CURRENT_DISCLOSURES': {
				return 'There is no current disclosure.';
			}
			case 'PROPOSAL_DISCLOSURES': {
				return 'There are no proposal disclosures.';
			}
			case 'TRAVEL_DISCLOSURES': {
				return 'There are no travel disclosures.';
			}
			case 'SIGNIFICANT_FINANCIAL_INTERESTS': {
				return 'There are no significant financial interests.';
			}
		}
	}

	activateDeactivate(sfi) {
		this.currentSFI = sfi;
		$('#activeConfirmation').modal('show');
	}

	searchList() {
		this.tempDisclosureArray = this.disclosureArray;
		if (this.searchId && this.searchName) {
			this.disclosureArray = this.disclosureArray.filter(ele => ele.coiEntityName.toLowerCase().includes(this.searchName.toLowerCase()) &&
			ele.coiFinancialEntityId.toString().includes(this.searchId));
		} else if (!this.searchId && this.searchName) {
			this.disclosureArray = this.disclosureArray.filter(ele => ele.coiEntityName.toLowerCase().includes(this.searchName.toLowerCase()));
		} else if (this.searchId && !this.searchName) {
			this.disclosureArray = this.disclosureArray.filter(ele =>
			ele.coiFinancialEntityId.toString().includes(this.searchId));
		}
	}

	clearSearch() {
		this.sfiDashboardRequestObject = new SFIDashboardRequestObject();
		this.fetchDashboardData();
	}

	getReviewStatusBadge(statusCode) {
        switch (statusCode) {
            case '1': return 'warning';
            case '2': return 'info';
            case '3': return 'success';
            default: return 'danger';
        }
    }
    getDisclosureStatusBadge(statusCode) {
        switch (statusCode) {
            case 1: return 'warning';
            case 2:
            case 4:
            case 5:
                return 'info';
            case 3: case 6: return 'success';
            default: return 'danger';
        }
    }
    getDispositionStatusBadge(statusCode) {
        switch (statusCode) {
            case 1: return 'warning';
            case 2:
            case 3: return 'success';
            default: return 'info';
        }
    }

	performAdvanceSearch() {
		this.sfiDashboardRequestObject.advancedSearch = 'A';
		this.sfiDashboardRequestObject.property17 =
			this.sfiDashboardRequestObject.property17 ? this.sfiDashboardRequestObject.property17 === 'true' : null;
		this.sfiDashboardRequestObject.property18 =
			this.sfiDashboardRequestObject.property18 ? this.sfiDashboardRequestObject.property18 === 'true' : null;
		this.sfiDashboardRequestObject.property19 =
			this.sfiDashboardRequestObject.property19 ? this.sfiDashboardRequestObject.property19 === 'true' : null;
		this.fetchDashboardData();
	}

	clearDisclosureCard() {
		this.isShowDisclosureInfo = false;
	}

	clearDisclosureInfo() {
		this.isShowTabInfo = false;
	}

	createDisclosure() {
		switch (this._dataStore.currentDashboardTab) {
			case 'CURRENT_DISCLOSURES': {
				this._router.navigate(['/fibi/coi/screening-questionnaire'], {
					queryParams: {
						tabName: this._dataStore.currentDashboardTab
					}
				});
				break;
			}
			case 'PROPOSAL_DISCLOSURES': {
				$('#createProposalDisclosureModal').modal('show');
				break;
			}
			case 'TRAVEL_DISCLOSURES': break;
			default: break;
		}
	}

	selectedProposal(event) {
		if (event) {
		  this.proposalDetails = event;
		  this.proposalDetails.proposalId = event.moduleItemId;
		  this.isShowResultCard =  true;
		} else {
			this.clearSearchFields();
		}
	  }

	clearSearchFields() {
		this.clearProposalField = new String('true');
		this.clearFieldSponsorField = new String('true');
		this.clearPIField = new String('true');
		this.clearLUField = new String('true');
		this.isShowResultCard = false;
		this.proposalDetails = {};
		this.ProposalSearchOptions = getEndPointOptionsForProposalDisclosure();
        this.projectDisclosureValidation.clear();
	}

	createDisclosureAPI() {
        if (this.validateProject()) {
            this.$subscriptions.push(this._coiDashboardService.createDisclosure({
                moduleItemId: this.proposalDetails.moduleItemId,
                moduleCode: this._dataStore.currentDashboardTab === 'PROPOSAL_DISCLOSURES' ? 3 : '',
                disclosureCategoryType: this._dataStore.currentDashboardTab
            }).subscribe((data: any) => {
                if (data) {
                    $('#createProposalDisclosureModal').modal('hide');
                    this._router.navigate(['/fibi/coi/screening-questionnaire'], {
                        queryParams: {
                            disclosureId: data.coiDisclosure.disclosureId
                        }
                    });
                }
            }, _err => {
                this._commonService.showToast(HTTP_ERROR_STATUS, 'Error in creating proposal disclosure. Please try again.');
            }));
        }
    }

	clearDisclosureModal() {
		this.isShowResultCard =  false;
		this.clearProposalField = new String('true');
		this.clearFieldSponsorField = new String('true');
		this.clearPIField = new String('true');
		this.clearLUField = new String('true');
		this.proposalDetails = {};
		this.proposalDetails.moduleItemId = null;
		this.proposalDetails.title = null;
        this.projectDisclosureValidation.clear();
    }

    validateProject() {
        this.projectDisclosureValidation.clear();
        if (!this.proposalDetails || !this.proposalDetails.moduleItemId) {
            this.projectDisclosureValidation.set('proposalSearch', 'Please select a proposal to create disclosure.');
        }
        return this.projectDisclosureValidation.size === 0 ? true : false;
    }

	getEventType(disclosureSequenceStatusCode, disclosureCategoryType) {
        if (disclosureCategoryType == 1) {
            if (disclosureSequenceStatusCode == 2 || disclosureSequenceStatusCode == 1 && !this.isActiveDisclosureAvailable) {
                return 'Active';
            } else if (disclosureSequenceStatusCode == 1 && this.isActiveDisclosureAvailable) {
                return 'Revision';
            }
        } else if (disclosureCategoryType == 3) {
            return 'Proposal';
        }
    }
}

