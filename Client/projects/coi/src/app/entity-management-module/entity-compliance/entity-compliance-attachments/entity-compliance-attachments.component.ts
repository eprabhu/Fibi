import { Component, Input, OnInit } from '@angular/core';

@Component({
    selector: 'app-entity-compliance-attachments',
    templateUrl: './entity-compliance-attachments.component.html',
    styleUrls: ['./entity-compliance-attachments.component.scss']
})
export class EntityComplianceAttachmentsComponent implements OnInit {

    @Input() sectionName: any;
    @Input() sectionId: any;
    entityAttachmentHelpText: string = '';
    isOpenSponsorAttachmentModal = false;

    constructor() { }

    ngOnInit() {
    }

    closeModal(event: any): void {
        this.isOpenSponsorAttachmentModal = false;
        this.entityAttachmentHelpText = '';
    }

    openAttachmentModal() {
        this.isOpenSponsorAttachmentModal = true
        this.entityAttachmentHelpText = 'You can view and edit attachments under the Compliance tab.';
    }

}
