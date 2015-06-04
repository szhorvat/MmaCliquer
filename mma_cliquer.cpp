
#include "WolframLibrary.h"

extern "C" {
#include "cliquer/cliquer.h"
}

#include <algorithm>

long *distribution;

boolean collector(set_t s, graph_t *, clique_options *) {
	distribution[set_size(s)-1]++;
	return TRUE;
}

extern "C" DLLEXPORT mint WolframLibrary_getVersion() {
	return WolframLibraryVersion;
}

extern "C" DLLEXPORT int WolframLibrary_initialize(WolframLibraryData libData) {
	distribution = NULL;
	return 0;
}

extern "C" DLLEXPORT void WolframLibrary_uninitialize(WolframLibraryData libData) { }

extern "C" DLLEXPORT int m_clique_distribution(WolframLibraryData libData, mint Argc, MArgument *Args, MArgument Res) {
	int n   = MArgument_getInteger(Args[0]);       // number of vertices
	int min = MArgument_getInteger(Args[1]);       // minimum clique size
	int max = MArgument_getInteger(Args[2]);       // maximum clique size

    boolean maximal_only = MArgument_getInteger(Args[3]); // find maximal cliques

	MTensor edges = MArgument_getMTensor(Args[4]); // edge list as integer matrix
	mint const *dims = libData->MTensor_getDimensions(edges);

	if (dims[1] != 2)
		return LIBRARY_DIMENSION_ERROR;

	mint const *edgesData = libData->MTensor_getIntegerData(edges);

	// create Cliquer graph
	graph_t *g = graph_new(n);
	for (int i=0; i < dims[0]; ++i) {
		GRAPH_ADD_EDGE(g, edgesData[2*i], edgesData[2*i+1]);
	}

    distribution = new long[max];
    std::fill(distribution, distribution + max, 0);

	clique_default_options->user_function = &collector;
	clique_unweighted_find_all(g, min, max, maximal_only, NULL);

	MTensor result;
	mint result_dims[] = { max };
	libData->MTensor_new(MType_Integer, 1, &(result_dims[0]), &result);
	std::copy(distribution, distribution + max, libData->MTensor_getIntegerData(result));
	MArgument_setMTensor(Res, result);

	// clean up
    delete [] distribution;
    distribution = NULL;
	graph_free(g);

	return LIBRARY_NO_ERROR;
}
