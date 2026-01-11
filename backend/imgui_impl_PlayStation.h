#pragma once

#if not defined (__ORBIS__)
#error "Invalid Backend Target Included In Project"
#endif

//
#include "imgui.h" // IMGUI_IMPL_API

// forward defs to avoid including headers.
struct ScePadControllerInformation;
struct ScePadData;
struct SceMouseData;
struct SceImeEvent;

namespace sce
{
	namespace Gnm
	{
		class DrawCommandBuffer;
	}

	namespace Gnmx
	{
		class GfxContext;
		class LightweightGfxContext;
	}
}

//
using ImGuiDrawCommandBuffer = sce::Gnm::DrawCommandBuffer;
using ImGuiGfxContext = sce::Gnmx::GfxContext;
using ImGuiLightweightGfxContext = sce::Gnmx::LightweightGfxContext;

template <class T>
struct ImGui_InputBase
{
public:
	using ReadData = bool(T**, int*);
	using ReadInfo = bool(void*);
public:
	ImGui_InputBase() = delete;

	ImGui_InputBase(ReadData* a_readFunc, ReadInfo* a_readInfo) :
		_readFunc(a_readFunc),
		_readInfo(a_readInfo)
	{}

	~ImGui_InputBase() = default;

	bool Poll()
	{
		if (_readFunc)
		{
			return _readFunc(&_data, &_dataSize);
		}
		else
		{
			return false;
		}
	}

	bool PollInfo(void* info)
	{
		if (_readInfo)
		{
			return _readInfo(info);
		}
		else
		{
			return false;
		}
	}

	int32_t count() const { return _dataSize; }
	T*      data() const { return _data; }
private:
	int32_t	  _dataSize = 0;
	T*        _data = nullptr;
	ReadData* _readFunc = nullptr;
	ReadInfo* _readInfo = nullptr;
};

// ImGui Relies on the user to provide a allocator that can allocate and free
class ImGui_Allocator
{
public:
	using allocate_t = void*(void*, unsigned int, unsigned int);
	using free_t = void(void*, void*);

	ImGui_Allocator() = default;

	ImGui_Allocator(void* a_instance, allocate_t* a_alloc, free_t* a_free) :
		_instance(a_instance),
		_allocate(a_alloc),
		_free(a_free)
	{}

	~ImGui_Allocator() = default;

	inline void* allocate(unsigned int a_size, unsigned int a_alignment)
	{
		auto ret = _allocate(_instance, a_size, a_alignment);
		return ret;
	}

	template <typename T>
	inline T* allocate()
	{ 
		return static_cast<T*>(_allocate(_instance, sizeof(T), 0));
	}

	inline void free(void* ptr)
	{
		return _free(_instance, ptr);
	}
public:
	void*		_instance = nullptr;
	allocate_t* _allocate = nullptr;
	free_t*		_free = nullptr;
};

struct ImGui_Allocators
{
	ImGui_Allocator onion;
	ImGui_Allocator garlic;
};

struct ImGui_InitUserData
{
	// Allocator ->
	ImGui_Allocators allocators;

	//
	bool EnableMouseSensitivity = false;
	bool EnableMouseAcceleration = false;	
	bool EnableMouseScaling = false;
	bool EnableMouseSmoothing = false;
	float MouseSensitivity = 1.0f;
	float MouseAccelerationFactor = 0.0f;

	// Input Sinks ->
	ImGui_InputBase<SceMouseData>* MouseInput;
	ImGui_InputBase<ScePadData>*   GamePadInput;
	ImGui_InputBase<SceImeEvent>*  KeyboardInput;
};

#ifndef IMGUI_DISABLE
IMGUI_IMPL_API bool ImGui_ImplPlayStation_Init(ImGui_InitUserData& a_allocators, unsigned int width, unsigned int height);
IMGUI_IMPL_API void ImGui_ImplPlayStation_Shutdown();
IMGUI_IMPL_API void ImGui_ImplPlayStation_NewFrame();

// #
/*
	- Uses a raw sce::Gnm::DrawCommandBuffer for it's rendering
*/
IMGUI_IMPL_API void ImGui_ImplPlayStation_RenderDrawData(ImGuiDrawCommandBuffer&, ImDrawData* draw_data);

/*
	- Uses sce::Gnmx::GfxContext for it's rendering
*/
IMGUI_IMPL_API void ImGui_ImplPlayStation_RenderDrawData(ImGuiGfxContext&, ImDrawData* draw_data);

/*
	- Uses sce::Gnmx::GfxContext for it's rendering
*/
IMGUI_IMPL_API void ImGui_ImplPlayStation_RenderDrawData(ImGuiLightweightGfxContext&, ImDrawData* draw_data);

/*
	- 
*/
IMGUI_IMPL_API void ImGui_ImplPlayStation_AddMouseEvent(float x, float y, float z);

/*
	-
*/
IMGUI_IMPL_API void ImGui_ImplPlayStation_SetSensitivity(float);
IMGUI_IMPL_API float ImGui_ImplPlayStation_GetSensitivity();
IMGUI_IMPL_API void ImGui_ImplPlayStation_SetMouseAccelerationEnabled(bool);
IMGUI_IMPL_API bool ImGui_ImplPlayStation_GetMouseAccelerationEnabled();

#endif // #ifndef IMGUI_DISABLE