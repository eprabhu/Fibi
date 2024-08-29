import { Component, Input, OnInit } from '@angular/core';
import { CommonService } from '../../../common/services/common.service';

@Component({
    selector: 'app-entity-sponsor-attachments',
    templateUrl: './entity-sponsor-attachments.component.html',
    styleUrls: ['./entity-sponsor-attachments.component.scss']
})
export class EntitySponsorAttachmentsComponent implements OnInit {

    @Input() sectionName: any;
    @Input() sectionId: any;
    entityAttachmentHelpText: string = '';
    isOpenSponsorAttachmentModal = false;

    constructor(public commonService: CommonService) { }

    ngOnInit() {
    }

    closeModal(event: any): void {
        this.isOpenSponsorAttachmentModal = false;
        this.entityAttachmentHelpText = '';
    }

    openAttachmentModal() {
        this.isOpenSponsorAttachmentModal = true
        this.entityAttachmentHelpText = 'You can view and edit attachments under the sponsor tab.';
    }
}
