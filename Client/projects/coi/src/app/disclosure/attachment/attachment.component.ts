import { Component, OnInit } from '@angular/core';
import {CoiService} from "../services/coi.service";

@Component({
  selector: 'app-attachment',
  templateUrl: './attachment.component.html',
  styleUrls: ['./attachment.component.scss']
})
export class AttachmentComponent implements OnInit {

  attachmentWarningMsg: string;
  uploadedFile = [];
  newAttachment = [];

  constructor(public _coiService: CoiService) { }

  ngOnInit() {
    this._coiService.isShowAttachmentInfo = true;
    window.scrollTo(0, 0);
  }

  closeAttachmentInfo() {
    this._coiService.isShowAttachmentInfo = false;
  }

  fileDrop(files) {
    this.attachmentWarningMsg = null;
    for (let index = 0; index < files.length; index++) {
        this.updateAddAttachmentDetails(files[index]);
    }

}

updateAddAttachmentDetails(file) {
    this.uploadedFile.push(file);
    // const attachment = new ExtReviewerAttachment();
    const attachment: any = {};
    attachment.fileName = file.name;
    attachment.mimeType = file.type;
    // attachment.externalReviewerId = this.externalReviewerDetails.extReviewerId;
    this.newAttachment.push(attachment);
}

clearAttachmentDetails() {
  setTimeout(() => {
      this.attachmentWarningMsg = null;
      this.uploadedFile = [];
      this.newAttachment = [];
  });
}
deleteFromUploadedFileList(index: number) {
  this.uploadedFile.splice(index, 1);
  this.attachmentWarningMsg = null;
  this.newAttachment.splice(index, 1);
}

}
