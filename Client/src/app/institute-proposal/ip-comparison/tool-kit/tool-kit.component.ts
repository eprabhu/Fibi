import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { HTTP_ERROR_STATUS } from '../../../app-constants';
import { CommonService } from '../../../common/services/common.service';
import { slideHorizontal } from '../../../common/utilities/animations';
import { scrollIntoView } from '../../../common/utilities/custom-utilities';
import { subscriptionHandler } from '../../../common/utilities/subscription-handler';
import { DataStoreService } from '../../services/data-store.service';
import { InstituteProposalService } from '../../services/institute-proposal.service';
import { IPSection } from '../comparison-constants';
import { CompareDetails, IPHistory, Section } from '../interface';
import { ToolkitInteractionService } from '../toolkit-interaction.service';
import { ToolKitService } from './tool-kit.service';

@Component({
	selector: 'app-tool-kit',
	templateUrl: './tool-kit.component.html',
	styleUrls: ['./tool-kit.component.css'],
	animations: [slideHorizontal]
})
export class ToolKitComponent implements OnInit, OnDestroy {

	isCurrentReviewTab = 'SECTION';
	$subscriptions: Subscription[] = [];
	proposalVersionsData: Array<IPHistory> = [];
	sections: Array<Section> = IPSection;
	scrollIntoView = scrollIntoView;
	leftVersion: IPHistory;
	rightVersion: IPHistory;
	isToolkitVisible = true;
	isCompareFlag = false;
	deBounceTimer: any;
	masterVersion: IPHistory;
	isMasterCompare = false;
	parameterValue: any;
	isShowMiniToolkit = false;
	ipHistories: any;

	constructor(
		private _toolKitService: ToolKitService,
        private _toolKitEvents: ToolkitInteractionService,
        public commonService: CommonService,
		public proposalService: InstituteProposalService,
		private _instituteProposalStore: DataStoreService
	) { }

	ngOnInit() {
		this.getIPDetails();
		this.getCompareValue();
		this.getCompareFromHeader();
		this.isToolkitVisible = this.commonService.isDevProposalVersioningEnabled ? true : false;
		this._toolKitEvents.$isToolkitVisible.next(this.isToolkitVisible);
		this.getToolkitVisibility();
	}

	private getIPDetails(): void {
		const IP = this._instituteProposalStore.getData(['instProposal']);
		this.$subscriptions.push(this._toolKitService.getIPHistory(IP.instProposal.proposalNumber)
			.subscribe((result: any) => {
				this.parameterValue = result.parameterValue;
				this.ipHistories = result.instituteProposalHistories;
				this.proposalVersionsData = this.formatProposalHistory(result.instituteProposalHistories);
				this.masterVersion = this.setActiveVersion(result.instituteProposalHistories);
				this.viewProposal(this.masterVersion);
			}));
	}

	private getCompareValue(): any {
		this.$subscriptions.push(
			this._toolKitEvents.$isCompareActive.subscribe(data =>
			this.isCompareFlag = data
		));
	}

	private getCompareFromHeader(): void {
		this.$subscriptions.push(this._toolKitEvents.$compareFromHeader.subscribe(data => {
			if (data) {
				this.checkForComparisonVersion();
			} else {
				this.viewProposal(this.rightVersion);
			}
		}));
	}

	private setActiveVersion(data): IPHistory {
		const ipId: any = data.find(ele => ele.proposalSequenceStatus === 'ACTIVE' || ele.proposalSequenceStatus === 'PENDING').proposalId;
		return {
			activeProposalId: ipId,
			proposalId: ipId,
			requestType: 'Active IP',
			createUserFullName: '',
			createTimestamp: '',
			versionNumber: 0
		};
	}

	scrollToSection(id: string): void {
		scrollIntoView(id);
	}

	private getToolkitVisibility(): void {
		this.$subscriptions.push(this._toolKitEvents.$isToolkitVisible.subscribe(data => {
			(this.isToolkitVisible = data) ? this.collapseToolKit() : this.expandToolKit();
		}));
	}

	private formatProposalHistory(data: any): Array<IPHistory> {
		const historyList: Array<IPHistory> = [];
		data.map((d, index) => {
			const history: any = {};
		    d.proposalSequenceStatus === 'ACTIVE' || d.proposalSequenceStatus === 'PENDING' ? history.activeProposalId = d.proposalId
																							: history.proposalId  = d.proposalId ;
			history.requestType = d.requestType;
			history.createUserFullName = d.createUserFullName;
			history.createTimestamp = d.createTimestamp;
			history.versionNumber = d.sequenceNumber;
			historyList.push(history);
		});
		return historyList;
	}

	viewProposal(version): void {
		this.leftVersion = version;
		this.rightVersion = null;
		this.setHeader(version, null);
		const ViewData: CompareDetails = {
			baseProposalId: version.proposalId.toString(),
			currentProposalId: ''
		};
		this._toolKitEvents.$viewEvent.next(ViewData);
		this._toolKitEvents.$isCompareActive.next(false);
	}

	private compareProposalVersions(): void {
		this.setHeader(this.leftVersion, this.rightVersion);
		const CompareData: CompareDetails = {
			baseProposalId: this.leftVersion.proposalId.toString(),
			currentProposalId: this.rightVersion.proposalId.toString()
		};
		this._toolKitEvents.$compareEvent.next(CompareData);
	}

	private setHeader(leftVersion = null, rightVersion = null): void {
		const CompareVersions = {
			leftVersion: leftVersion || this.leftVersion,
			rightVersion: rightVersion || {},
		};
		this._toolKitEvents.$currentHeader.next(CompareVersions);
	}

	updateToolkitView(): void {
		this.isToolkitVisible = !this.isToolkitVisible;
		this._toolKitEvents.$isToolkitVisible.next(this.isToolkitVisible);
	}

	expandToolKit(): void {
		(document.getElementById('ip_compare_review') as HTMLElement).style.width = '100%';
	}

	collapseToolKit(): void {
		(document.getElementById('ip_compare_review') as HTMLElement).style.width = '75%';
	}

	private checkForComparisonVersion(): void {
		if (this.leftVersion) {
			this.rightVersion = this.leftVersion;
			this.leftVersion = this.setActiveVersion(this.ipHistories);
			this.compareProposalVersions();
		} else {
			this.commonService.showToast(HTTP_ERROR_STATUS, 'No previous version available to compare');
			this._toolKitEvents.$isCompareActive.next(false);
		}
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}


}
