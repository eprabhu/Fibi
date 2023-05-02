import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { Subscription } from 'rxjs';
import {CountModalService} from "./count-modal.service";	

declare var $: any;

@Component({
	selector: 'app-count-modal',
	templateUrl: './count-modal.component.html',
	styleUrls: ['./count-modal.component.css'],
	providers: [CountModalService]
})
export class CountModalComponent implements OnInit {

	@Input() moduleCode: number;
	@Input() disclosureId: number;
	@Input() disclosureSequenceStatusCode: number;
	@Input() disclosureNumber: number;
	@Input() inputType: any;
	@Input() personId: any;
	@Input() disclosureType: any;
	@Output() closeModal: EventEmitter<boolean> = new EventEmitter<boolean>();
	$subscriptions: Subscription[] = [];
	tableArray: any[] = [];
	currentModalTab = 'Award';
	projectDatas: any;
	coiFinancialEntityDetails: any[] = [];

	constructor(private _countModalService: CountModalService) { }

	ngOnInit() {
		if (this.moduleCode === 8) {
			this.getSFIDatas();
		} else if (this.moduleCode === 101 && this.inputType === 'SFI_TAB') {
			this.getDisclosureDatas();
		} else {
			this.getProjectDatas();
		}
	}

	getSFIDatas() {
		this.$subscriptions.push(this._countModalService.getSFICount(this.disclosureId, this.disclosureSequenceStatusCode,this.personId).subscribe((data: any) => {
			this.coiFinancialEntityDetails = data;
			document.getElementById('hidden-open-button').click();
		}));
	}

	getProjectDatas() {
	if (this.inputType === 'DISCLOSURE_TAB') {
		this.$subscriptions.push
		(this._countModalService.getProjectsCount(this.disclosureId, this.disclosureSequenceStatusCode, this.personId).subscribe((data: any) => {
			this.projectDatas = data;
			this.currentModalTab = this.moduleCode === 1 ? 'Award' : 'Proposal';
			this.switchTableData();
			document.getElementById('hidden-open-button').click();
		}, err => {
			this.closeCountModal();
		}));
	} else {
		this.$subscriptions.push
		(this._countModalService.getAwardProposalSFIList(this.disclosureId).subscribe((data: any) => {
			this.projectDatas = data;
			this.currentModalTab = this.moduleCode === 1 ? 'Award' : 'Proposal';
			this.switchTableData();
			document.getElementById('hidden-open-button').click();
		}, err => {
			this.closeCountModal();
		}));
	}
	}

	getDisclosureDatas() {
		this.$subscriptions.push(this._countModalService.getDisclosureDetails(this.disclosureId).subscribe((data: any) => {
			this.coiFinancialEntityDetails = data;
			document.getElementById('hidden-open-button').click();
		}));
	}

	getModalHeader() {
		if (this.moduleCode === 8 && this.inputType === 'DISCLOSURE_TAB') {
			return `SFIs Attached to Disclosure: #${this.disclosureNumber} - ${this.disclosureType}`;
		} else if (this.moduleCode === 101 && this.inputType === 'SFI_TAB') {
			return `Disclosures attached to SFI: #${this.disclosureNumber} - ${this.disclosureType}`;
		} else if (this.moduleCode === 3 || this.moduleCode === 1) {
			return `Proposals, Awards and Protocols related to: #${this.disclosureNumber} - ${this.disclosureType}`;
		}
	}

	closeCountModal() {
		this.closeModal.emit(false);
	}

	switchTableData() {
		this.tableArray = this.currentModalTab === 'Proposal' ? this.projectDatas.proposals : this.projectDatas.awards;
	}

}
