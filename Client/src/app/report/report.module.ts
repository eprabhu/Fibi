import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReportComponent } from './report.component';
import { FormsModule } from '@angular/forms';
import { ReportService } from './report.service';
import { SharedModule } from '../shared/shared.module';
import { TemplateListComponent } from './template-list/template-list.component';
import { ReportsRoutingModule } from './report-routing.module';
import { TemplateComponent } from './template/template.component';



@NgModule({
  imports: [CommonModule,
            SharedModule,
            FormsModule,
            ReportsRoutingModule],
  declarations: [ReportComponent, TemplateListComponent, TemplateComponent],
  providers: [ReportService]
})
export class ReportModule { }
