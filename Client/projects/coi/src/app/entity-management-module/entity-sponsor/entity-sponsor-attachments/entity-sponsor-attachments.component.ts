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

    constructor(public commonService: CommonService) { }

    ngOnInit() {
    }

    closeModal() {
        this.commonService.isOpenAttachmentModal = false;
        this.entityAttachmentHelpText = '';
    }

    openAttachmentModal() {
        this.commonService.isOpenAttachmentModal = true
        this.entityAttachmentHelpText = 'You can view and edit attachments under the sponsor tab.';
    }
}
