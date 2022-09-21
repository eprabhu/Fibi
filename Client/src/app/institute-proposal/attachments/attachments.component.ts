import { Component, OnInit, OnDestroy } from '@angular/core';
import { InstituteProposalService } from '../services/institute-proposal.service';
import { Subscription } from 'rxjs';
import { subscriptionHandler } from '../../common/utilities/subscription-handler';
import { environment } from '../../../environments/environment';
import { AttachmentsService } from './attachments.service';
import { ActivatedRoute } from '@angular/router';
import { fileDownloader } from '../../common/utilities/custom-utilities';
import { Attachment, AttachmentType, InstituteProposal } from '../institute-proposal-interfaces';
import { DataStoreService } from '../services/data-store.service';
import { HTTP_ERROR_STATUS, HTTP_SUCCESS_STATUS } from '../../app-constants';
import { CommonService } from '../../common/services/common.service';
declare var $: any;

@Component({
	selector: 'app-attachments',
	templateUrl: './attachments.component.html',
	styleUrls: ['./attachments.component.css']
})
export class AttachmentsComponent implements OnInit, OnDestroy {

	attachmentVersions = [];
	isAttachmentEditable = true;
	$subscriptions: Subscription[] = [];
	deployMap = environment.deployUrl;
	sortBy = 'updateTimeStamp';
	order: number = -1;
	sortByDevProposal = 'updateTimeStamp';
	orderDevProposal: number = -1;
	instProposalId = null;
	attachment: Attachment = new Attachment();
	attachments: Array<Attachment> = [];
	devProposalAttachments: any = {}
	editIndex = -1;
	deleteIndex = -1;
	replaceIndex = -1;
	isReplaceAttachmentEnabled = false;
	isAttachmentListOpen = true;
	isDevProposalAttachmentListOpen = true;
	uploadedFile: Array<File> = [];
	errorMap = new Map();
	attachmentTypes: Array<AttachmentType> = []
	newAttachments: Array<Attachment> = []
	isSaving = false;
	fileName = '';
	isModifyProposal = false;
	editAttachmentDetails: any = {};
	devProposalAttachmentIds: Array<string> = [];
	ID: string = '';
	constructor(public _instituteService: InstituteProposalService,
		private _attachment: AttachmentsService,
		private _route: ActivatedRoute, private _dataStore: DataStoreService,
		private _commonService: CommonService) { }

	ngOnInit() {
		this.getProposalAttachments()
		this.getGeneralDetails();
	}

	ngOnDestroy() {
		subscriptionHandler(this.$subscriptions);
	}

	getGeneralDetails() {
		const data: InstituteProposal = this._dataStore.getData(['proposalAttachmentTypes', 'isReplaceAttachmentEnabled', 'availableRights']);
		this.attachmentTypes = data.proposalAttachmentTypes;
		this.isReplaceAttachmentEnabled = data.isReplaceAttachmentEnabled;
		this.isModifyProposal = data.availableRights.includes('MODIFY_INST_PROPOSAL')
	}

	getProposalAttachments() {
		this.instProposalId = this._route.snapshot.queryParamMap.get('instituteProposalId');
		this.$subscriptions.push(this._attachment.getProposalAttachments(this.instProposalId)
			.subscribe((data: InstituteProposal) => {
				this.attachments = data.instituteProposalAttachments;
				this.devProposalAttachments = data.proposalAttachments || {};
				this.devProposalAttachmentIds = Object.keys(data.proposalAttachments);
				this.ID = this.devProposalAttachmentIds[0];
			}));
	}

	downloadProposalAttachments(attachment) {
		this.$subscriptions.push(this._attachment.downloadProposalAttachment(attachment.attachmentId)
			.subscribe(data => {
				fileDownloader(data, attachment.fileName);
			}));
	}

	downloadDevProposalAttachments(attachment) {
		this.$subscriptions.push(this._attachment.downloadDevProposalAttachments(attachment.attachmentId)
			.subscribe(data => {
				fileDownloader(data, attachment.fileName);
			}));
	}

	getVersion(documentId: number, proposalId: number, fileName: string) {
		this.fileName = fileName;
		this.attachmentVersions = this.attachments.filter(A =>
			A.documentStatusCode === 2 && A.documentId === documentId && A.proposalId === proposalId);
	}

	addAttachments(): void {
		if (this.validateAttachments() && !this.isSaving) {
			const ID = parseInt(this._route.snapshot.queryParamMap.get('instituteProposalId'), 10);
			this.$subscriptions.push(this._attachment.addProposalAttachment(this.uploadedFile, ID, this.newAttachments)
				.subscribe((data: InstituteProposal) => {
					this.isSaving = false;
					this.attachments = data.instituteProposalAttachments;
					this._commonService.showToast(HTTP_SUCCESS_STATUS, this.replaceIndex > -1 ? 'Attachment replaced successfully':'Attachment added successfully');
					$('#addAwardAttachment').modal('hide');
					this.newAttachments = [];
					this.uploadedFile = [];
					this.replaceIndex = -1;
				}, err => {
					this._commonService.showToast(HTTP_ERROR_STATUS, this.replaceIndex > -1 ? 'Failed to replace Attachment' : 'Failed to add Attachment');
					this.isSaving = false;
				}))
		}
	}

	setEditAttachment(index: number, attachment): void {
		this.editAttachmentDetails = JSON.parse(JSON.stringify(attachment));
		$('#editIpAttachmentModal').modal('show');
		this.editIndex = index;
	}

	updateAttachmentDescription(): void {
		this.$subscriptions.push(this._attachment.updateIPAttachmentDetails({ instituteProposalAttachment: this.editAttachmentDetails })
			.subscribe((data: any) => {
				this.attachments = data;
				this.editIndex = -1;
				this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Attachment updated successfully');
			}));
			$('#editIpAttachmentModal').modal('hide');
	}

	clearEditAttachmentDetails() {
		this.editAttachmentDetails = {narrativeStatus: {description: ''}};
		$('#editIpAttachmentModal').modal('hide');
	  }

	OnFileDrop(files) {
		if(this.replaceIndex > -1) {
			this.uploadedFile = [];
			this.newAttachments = [];
		}
		for (let index = 0; index < files.length; index++) {
			this.uploadedFile.push(files[index]);
			this.replaceIndex > -1 ? this.updateAttachments(files[index].name) : this.addNewAttachments(files[index].name)
		}
	}

	updateAttachments(fileName: string) {
		let attachment: Attachment = JSON.parse(JSON.stringify(this.attachments[this.replaceIndex]))
		attachment.fileName = fileName;
		this.newAttachments.push(attachment);
	}

	addNewAttachments(fileName: string) {
		let attachment = new Attachment();
		attachment.proposalId = parseInt(this._route.snapshot.queryParamMap.get('instituteProposalId'), 10);
		attachment.fileName = fileName;
		this.newAttachments.push(attachment);
	}

	cancelAttachmentAction() {
		setTimeout(() => {
			this.uploadedFile = [];
			this.replaceIndex = -1;
			this.newAttachments = [];
			this.errorMap.clear();
		});
	}

	onAttachmentTypeChange(index: number, typeCode: number | string) {
		const TYPE = this.attachmentTypes.find(A => A.attachmentTypeCode == typeCode);
		this.newAttachments[index].attachmentType = TYPE;
	}

	clearAttachmentDetails(index: number) {
		this.newAttachments.splice(index, 1)
		this.uploadedFile.splice(index, 1)
		this.validateAttachments();
	}

	validateAttachments(): boolean {
		this.errorMap.clear();
		if (this.checkMandatoryFields()) {
			this.errorMap.set('mandatory', '* Please fill all the mandatory fields');
		}
		return this.errorMap.size > 0 ? false : true;

	}

	checkMandatoryFields(): boolean {
		return !!this.newAttachments.find(NA => NA.attachmentTypeCode == 'null' || !NA.attachmentTypeCode);
	}

	deleteAttachment(): void {
		const ID = parseInt(this._route.snapshot.queryParamMap.get('instituteProposalId'), 10);
		const attachmentId = this.attachments[this.deleteIndex].attachmentId;
		this.$subscriptions.push(this._attachment.deleteAttachment(ID, attachmentId)
			.subscribe(data => {
				this.attachments.splice(this.deleteIndex, 1);
				this._commonService.showToast(HTTP_SUCCESS_STATUS, 'Attachment deleted successfully');
				this.deleteIndex = -1
			}))
	}

	sort(property: string) {
		this.order = this.order * -1;
		this.sortBy = property;
	}

	sortDevProposal(property: string) {
		this.orderDevProposal = this.orderDevProposal * -1;
		this.sortByDevProposal = property;
	}

	sortNull() { return 0; }
}
