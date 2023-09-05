
import { Component, Input, OnInit } from '@angular/core';
import { Observable, Subject } from 'rxjs';
import { DataService } from './data.service';
import * as d3 from 'd3';
import { GraphEvent, GraphDetail, GraphDataRO, HEIGHT, WIDTH, DISTANCE_BTN_NODES, FORCE_BTN_NODES } from './interface';

@Component({
    selector: 'app-graph',
    templateUrl: './graph.component.html',
    styleUrls: ['./graph.component.scss'],
    providers: [DataService]
})
export class GraphComponent implements OnInit {

    @Input() graphEnableEvent: Observable<GraphDetail>;
    @Input() graphId: string | number;
    graphDetail: GraphDetail = new GraphDetail();
    graphNodeEvents = new Subject<GraphEvent>;
    popOverEvents = new Subject<boolean>;
    popOverPositions = {
        clientX: 0,
        clientY: 0,
        popoverHeight: 400,
        popoverWidth: 500
    };
    selectedRelations: any = {};
    cardData: any = {};
    relations = [];
    height = HEIGHT;
    width = WIDTH;
    graph = { nodes: [], links: [] };
    svg;
    simulation;
    link;
    node;
    zoom;
    isShowFilter = true;

    constructor(public graphDataService: DataService) { }

    async ngOnInit(): Promise<any> {
        if (this.graphId) {
            await this.graphDataService.getGraphConfiguration(this.graphId).catch(err => {
                // do something here to handle error and indicate the error.
            });
        }
        await this.graphDataService.getMetaDataForGraph().catch(err => {
            // do something here to handle error and indicate the error.
        });
        this.manageGraphDisplay();
        this.listenToGraphEvents();
    }

    private manageGraphDisplay(): void {
        this.graphEnableEvent.subscribe(async (data: GraphDetail) => {
            this.graphDetail = data;
            if (data.visible) {
                this.setSVGForGraph();
                this.toggleGraphModal(true);
                const RO: GraphDataRO = this.fetchROForRoot(100151);
                const GRAPH_DATA = await this.graphDataService.getDataForGraph(RO);
                this.addNodesAndLinks(GRAPH_DATA.nodes, GRAPH_DATA.links);
            } else {
                this.toggleGraphModal(false);
            }
        });
    }

    drawGraph() {
        this.link = this.svg.selectAll('.link').data(this.graph.links);
        this.link.enter()
            .insert('line', '.node')
            .attr('class', 'link')
            .style('stroke', '#d9d9d9')
            .attr('marker-end', 'url(#arrow)');
        this.link.exit().remove();

        this.node = this.svg.selectAll('.node').data(this.graph.nodes);
        const g = this.node.enter().append('g').attr('class', 'node');

        g.append('circle')
            .attr('r', 20)
            .attr('fill', d => `url(#${d.label})`);

        g.append('text')
            .style('font-size', '10px')
            .attr('x', 20)
            .attr('y', '0.50em')
            .text(d => this.getTextForNode(d))
            .clone(true).lower()
            .attr('fill', 'none')
            .attr('stroke', 'white')
            .attr('stroke-width', 2);

        this.node.call(d3.drag()
            .on('start', this.dragStarted.bind(this))
            .on('drag', this.dragged.bind(this))
            .on('end', this.dragEnded.bind(this)));
        this.node.exit().remove();

        this.attachEventsForGraph();

        this.simulation.nodes(this.graph.nodes)
            .force('link', d3.forceLink(this.graph.links).distance(100))
            .force('charge', d3.forceManyBody().strength(-500))
            .alpha(0.03)
            .restart();
    }

    private setSVGForGraph(): void {
        this.attachZoom();
        this.svg = d3.select('#chart-container').append('svg')
            .attr('viewBox', [-WIDTH / 2.5, -HEIGHT / 3.8, WIDTH, HEIGHT])
            .call(this.zoom).append('svg:g');
        this.zoom.scaleTo(this.svg, 1 / 1);
        this.attachStimulation();
        this.graphDataService.setImageMarkersForGraph(this.svg);
        this.graphDataService.assignArrowheadToGraphLinks(this.svg);
    }

    private attachStimulation() {
        this.simulation = d3.forceSimulation()
            .force('link', d3.forceLink(this.graph.links).id((d: any) => d.elementId))
            .force('charge', d3.forceManyBody().strength(FORCE_BTN_NODES))
            .force('center', d3.forceCenter().strength(1))
            .force('x', d3.forceX())
            .force('y', d3.forceY())
            .force('collide', d3.forceCollide(d => DISTANCE_BTN_NODES).strength(0.01))
            .on('tick', () => {
                this.svg.selectAll('.link')
                    .attr('x1', (d) => d.source.x)
                    .attr('y1', (d) => d.source.y)
                    .attr('x2', (d) => d.target.x)
                    .attr('y2', (d) => d.target.y);
                this.svg.selectAll('.node')
                    .attr('cx', (d) => d.x)
                    .attr('cy', (d) => d.y)
                    // tslint:disable-next-line:quotemark
                    .attr('transform', (d) => "translate(" + d.x + "," + d.y + ")");
            });
    }

    private attachZoom() {
        this.zoom = d3.zoom()
            .scaleExtent([1 / 2, 10])
            .on('zoom', d => this.svg.attr('transform', d.transform));
    }

    private dragStarted(event) {
        if (!event.active) { this.simulation.alphaTarget(0.03).restart(); }
        event.subject.fx = event.subject.x;
        event.subject.fy = event.subject.y;
    }

    private dragged(event, d) {
        event.subject.fx = event.x;
        event.subject.fy = event.y;
    }

    private dragEnded(event, d) {
        if (!event.active) { this.simulation.alphaTarget(0); }
        event.subject.fx = null;
        event.subject.fy = null;
    }

    private toggleGraphModal(visible: boolean): void {
        visible ? document.getElementById('d3GraphModalbutton').click() : document.getElementById('graph-modal-dismiss-btn').click();
    }

    private fetchROForRoot(id): GraphDataRO {
        const RO: GraphDataRO = this.getROForGraph(this.graphDataService.graphTypeConfiguration.root_node[0],
            this.graphDetail.id, this.graphDataService.graphTypeConfiguration.root_relations);
        return RO;
    }

    private getROForGraph(node: string, value: string, relations: string[]): GraphDataRO {
        const RO = new GraphDataRO();
        RO.value = value;
        RO.relationship = relations;
        RO.node = node;
        return RO;
    }

    addNodes(node) {
        if (!this.graph.nodes.find(N => N.elementId === node.elementId)) {
            node['id'] = node['elementId'];
            this.graph.nodes.push(node);
            this.drawGraph();
        }
    }

    addLinks(link) {
        if (!this.graph.links.find(L => L.source.elementId === link.source && L.target.elementId === link.target
            && L.type === link.type)) {
            const indexes = this.findSourceAndTargetIndex(link);
            link = { ...link, ...indexes };
            this.graph.links.push(link);
            this.drawGraph();
        }
    }

    addNodesAndLinks(nodes = [], links = []): void {
        nodes.forEach(N => this.addNodes(N));
        links.forEach(L => this.addLinks(L));
    }


    findSourceAndTargetIndex(link): any {
        const source = this.graph.nodes.findIndex(N => N.elementId === link.source);
        const target = this.graph.nodes.findIndex(N => N.elementId === link.target);
        return { source, target };
    }

    private getTextForNode(node): string {
        switch (node.label) {
            case 'Award': return node.award_number;
            case 'COIDisclosure': return node.disclosure_number;
            case 'Country': return node.country_name;
            case 'Entity': return node.name;
            case 'Person': return node.full_name;
            case 'Proposal': return node.title;
            case 'Sponsor': return node.sponsor_name;
            case 'TravelDisclosure': return node.travel_number;
            case 'Unit': return node.unit_name;
        }
    }

    private attachEventsForGraph(): void {
        this.node.on('click', (event, d) => {
            this.selectedRelations[d.elementId] = this.selectedRelations[d.elementId] || {};
            this.graphNodeEvents.next({ index: d.index, clientX: event.clientX, clientY: event.clientY });
            console.log(d);
            this.cardData = d;
            this.relations = this.setConnectionsDataForPopUp(d) || [];
            console.log(this.relations);
        });
        this.link.on('click', (event, d) => {
            console.log('Click from link');
        });
    }

    setConnectionsDataForPopUp(node) {
        const data = this.graphDataService.graphTypeConfiguration.relations.find(R => R.node === node.label);
        return data?.relationships;
    }



    private listenToGraphEvents() {
        this.graphNodeEvents.subscribe((data: any) => { this.showBasicDetailsPopup(data); });
    }


    private showBasicDetailsPopup(position) {
        this.popOverPositions.clientX = position.clientX;
        this.popOverPositions.clientY = position.clientY;
        this.popOverEvents.next(true);
    }

    async drillDownEvent(event, relation): Promise<any> {
        console.log(this.selectedRelations);
        const value = this.getValueForNode(this.cardData);
        const RO: GraphDataRO = this.getROForGraph(this.cardData.label, value, [relation]);
        const GRAPH_DATA = await this.graphDataService.getDataForGraph(RO);
        this.addNodesAndLinks(GRAPH_DATA.nodes, GRAPH_DATA.links);
        this.hideBasicDetailsPopup();
    }

    private getValueForNode(node: any): string {
        const valueFiled = this.graphDataService.graphMetaData.nodes[node.label].valueField;
        switch (node.label) {
            case 'Award': return node[valueFiled];
            case 'COIDisclosure': return node[valueFiled];
            case 'Country': return node[valueFiled];
            case 'Entity': return node[valueFiled];
            case 'Person': return node[valueFiled];
            case 'Proposal': return node[valueFiled];
            case 'Sponsor': return node[valueFiled];
            case 'TravelDisclosure': return node[valueFiled];
            case 'Unit': return node[valueFiled];
        }
    }

    private hideBasicDetailsPopup(): void {
        this.popOverEvents.next(false);
    }

    clearGraph() {
        document.getElementById('chart-container').innerHTML = '';
        this.graph = { nodes: [], links: [] };
    }

}
